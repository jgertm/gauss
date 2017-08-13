{-# LANGUAGE DeriveGeneric #-}

module Gauss.Expressions.Input where

import           Gauss.Operations


data Expression op args dom =
    Literal dom
  | Symbol String
  | Application op args
  deriving (Generic)


instance (Show op, Show args, Show dom)
       => Show (Expression op args dom) where
  show (Literal l)          = printf "[Lit %s]" $ show l
  show (Symbol s)           = printf "[Sym %s]" s
  show (Application op exs) = printf "[Apl %s %s]" (show op) (show exs)

instance IsString (Expression op args dom) where
  fromString s = Symbol s


plus :: ((Expression lop largs ldom, Expression rop rargs rdom) ~ args, Codomain Addition (ldom,rdom) ~ dom)
     => Expression lop      largs ldom
     -> Expression rop      rargs rdom
     -> Expression Addition  args  dom
plus a b = Application Addition (a,b)

times :: ((Expression lop largs ldom, Expression rop rargs rdom) ~ args, Codomain Multiplication (ldom,rdom) ~ dom)
      => Expression lop            largs ldom
      -> Expression rop            rargs rdom
      -> Expression Multiplication  args  dom
times a b = Application Multiplication (a,b)

minus :: ((Expression lop largs ldom, Expression rop rargs rdom) ~ args, Codomain Subtraction (ldom,rdom) ~ dom)
      => Expression lop         largs ldom
      -> Expression rop         rargs rdom
      -> Expression Subtraction  args  dom
minus a b = Application Subtraction (a,b)

over :: ((Expression lop largs ldom, Expression rop rargs rdom) ~ args, Codomain Division (ldom,rdom) ~ dom)
     => Expression lop      largs ldom
     -> Expression rop      rargs rdom
     -> Expression Division  args  dom
over a b = Application Division (a,b)
