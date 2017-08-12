{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Gauss.Expressions.Input where

import           Gauss.Operations

import           GHC.Generics

-- import           GHC.Exts         (Constraint)


data Expression dom where
  Literal :: (Typeable dom)
          => dom
          -> Expression dom

  Symbol :: (Typeable dom)
         => String
         -> Expression dom

  Application :: ( Operation op args
                 , Codomain op args ~ dom
                 , Lift Expression args ~ liftedArgs
                 , Codomain op liftedArgs ~ Expression dom --, Operation op liftedArgs
                 , Show liftedArgs, Show dom, Typeable dom
                 )
              => op
              -> liftedArgs
              -> Expression dom

instance Generic (Expression dom) where

type EI = Expression Integer

instance (Operation op (a,b))
       => Operation op (Expression a, Expression b) where
  type Codomain op (Expression a, Expression b) = Expression (Codomain op (a,b))
  -- evaluate op (Literal a, Literal b) = Literal $ evaluate op (a,b)
  evaluate _  _                      = undefined

instance (Show dom)
       => Show (Expression dom) where
  show (Literal l)          = "(L " <> show l <> ")"
  show (Symbol s)           = "(S " <> s <> ")"
  show (Application op exs) = "(A " <> show op <> show exs

instance (Num n, Show n, Ring n, Typeable n)
       => Num (Expression n) where
  a + b = Application Addition (a,b)
  a * b = Application Multiplication (a,b)
  fromInteger i = Literal $ fromInteger i

instance (Typeable n)
       => IsString (Expression n) where
  fromString s = Symbol s
