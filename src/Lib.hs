{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lib where

import ClassyPrelude

data Sym = Sym String
  deriving (Show)

data Con c where
  Con :: (Atom c) => c -> Con c
instance Show (Con c) where
  show (Con c) = show c

data App op args ret where
  App :: (Operation op args, ret ~ Codomain op args) =>
         op -> args -> App op args ret
deriving instance Show (App op args ret)

class Expr ex
instance Expr Sym
instance Expr (Con c)
instance Expr (App op args ret)

class (Show op,Show args) =>
      Operation op args  where
  type Codomain op args :: *
instance (Operation op (ret1,ret2)) =>
         Operation op (Con ret1,Con ret2) where
  type Codomain op (Con ret1,Con ret2) = Codomain op (Return (Con ret1),Return (Con ret2))
instance (Operation op (ret1,ret2)) =>
         Operation op (App op1 args1 ret1,App op2 args2 ret2) where
  type Codomain op (App op1 args1 ret1,App op2 args2 ret2) = Codomain op (ret1,ret2)
-- instance (Operation op (ret1, ret2), Eval arg1, Eval arg2)
--          => Operation op (arg1, arg2) where
--   type Codomain op (arg1, arg2) = Codomain op (Return arg1, Return arg2)

data Addition = Addition
instance Show Addition where
  show _ = "add"
instance Operation Addition (Integer,Integer) where
  type Codomain Addition (Integer,Integer) = Integer

data Multiplication = Multiplication
instance Show Multiplication where
  show _ = "multiply"
instance Operation Multiplication (Integer,Integer) where
  type Codomain Multiplication (Integer,Integer) = Integer
instance Operation Multiplication (Double,Double) where
  type Codomain Multiplication (Double,Double) = Double

class Eval x  where
  type Return x :: *
  eval :: x -> Return x
instance Eval (Con c) where
  type Return (Con c) = c
  eval (Con c) = c
instance Eval (App op args ret) where
  type Return (App op args ret) = ret
  eval (App op args) = undefined

class (Show a) =>
      Atom a
instance Atom Int
instance Atom Integer
instance Atom Double

exOne :: Con Integer
exOne = Con 3

exTwo :: App Multiplication (Con Integer,Con Integer) Integer
exTwo = App Multiplication (exOne,exOne)

exThree :: Con Double
exThree = Con 3.2

exFour :: App Multiplication (Con Double,Con Double) Double
exFour = App Multiplication (exThree,exThree)
