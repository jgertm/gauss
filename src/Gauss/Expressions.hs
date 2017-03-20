{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Gauss.Expressions where

import           Gauss.Operations

import           ClassyPrelude

import           GHC.Exts         (Constraint)


type Identifier = Int

data Expression dom where
  Literal :: dom
          -> Expression dom

  Symbol :: String
         -> Identifier
         -> Expression dom

  Application :: ( Operation op args
                 , Codomain op args ~ dom
                 , Lift Expression args ~ liftedArgs
                 , Codomain op liftedArgs ~ Expression dom, Operation op liftedArgs
                 , Show liftedArgs, Show dom
                 )
              => op
              -> liftedArgs
              -> Expression dom

type EI = Expression Integer

type family con :<$> tup = (res :: Constraint) where
  con :<$> (a,b)   = (con a, con b)
  con :<$> (a,b,c) = (con a, con b, con c)

instance (Operation op (a,b))
       => Operation op (Expression a, Expression b) where
  type Codomain op (Expression a, Expression b) = Expression (Codomain op (a,b))
  evaluate op (Literal a, Literal b) = Literal $ evaluate op (a,b)
  evaluate _ _ = undefined

instance (Show dom)
       => Show (Expression dom) where
  show (Literal l)          = "(L " <> show l <> ")"
  show (Symbol s i)         = "(S " <> s <> "_" <> show i <> ")"
  show (Application op exs) = "(A " <> show op <> show exs

instance (Num n, Show n, Ring n)
       => Num (Expression n) where
  a + b = Application Addition (a,b)
  a * b = Application Multiplication (a,b)
  fromInteger i = Literal $ fromInteger i

foo :: EI
foo = 2 + 3

reduce :: Expression t -> Maybe t
reduce (Application op exs) = let (Literal l) = evaluate op exs
                               in Just l
reduce _ = Nothing
