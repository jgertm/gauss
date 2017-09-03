{-# LANGUAGE AllowAmbiguousTypes #-}

module Gauss.Expressions.Input where

import           Gauss.Argument
import           Gauss.Operations


type SymbolID = String

data Expression op args dom where
  Literal     :: dom -> Expression () () dom

  Symbol      :: SymbolID -> Expression () () dom

  Application :: ( Operation op args
                 , Codomain op args ~ dom
                 , Lift (Expression x y) args ~ liftedArgs
                 , Show liftedArgs
                 )
              => op
              -> liftedArgs
              -> Expression op args dom

deriving instance (Show op, Show dom, Show args) => Show (Expression op dom args)


-- type Literal a = Expression () () a

-- instance (Show op, Show args, Show dom)
--        => Show (Expression op args dom) where
--   show (Literal l)          = printf "Lit %s" $ show l
--   show (Symbol s)           = printf "Sym %s" s
--   show (Application op exs) = printf "Apl %s %s" (show op) (show exs)

-- instance IsString (Expression op args dom) where
--   fromString s = Symbol s

-- literal :: val -> Literal val
-- literal = Literal


-- plus :: ((Expression lop largs ldom, Expression rop rargs rdom) ~ args, Codomain Addition (ldom,rdom) ~ dom)
--      => Expression lop      largs ldom
--      -> Expression rop      rargs rdom
--      -> Expression Addition  args  dom
-- plus a b = Application Addition (a,b)

-- times :: ((Expression lop largs ldom, Expression rop rargs rdom) ~ args, Codomain Multiplication (ldom,rdom) ~ dom)
--       => Expression lop            largs ldom
--       -> Expression rop            rargs rdom
--       -> Expression Multiplication  args  dom
-- times a b = Application Multiplication (a,b)

-- minus :: ((Expression lop largs ldom, Expression rop rargs rdom) ~ args, Codomain Subtraction (ldom,rdom) ~ dom)
--       => Expression lop         largs ldom
--       -> Expression rop         rargs rdom
--       -> Expression Subtraction  args  dom
-- minus a b = Application Subtraction (a,b)

-- over :: ((Expression lop largs ldom, Expression rop rargs rdom) ~ args, Codomain Division (ldom,rdom) ~ dom)
--      => Expression lop      largs ldom
--      -> Expression rop      rargs rdom
--      -> Expression Division  args  dom
-- over a b = Application Division (a,b)
