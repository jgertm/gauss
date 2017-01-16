{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gauss.Expressions where

import           Gauss.Operations

import           Data.Monoid
import qualified Data.Vector.Fixed       as V
import           Data.Vector.Fixed.Boxed


type Identifier = Int

data Expression e where
  Literal :: e
          -> Expression e

  Symbol  :: String
          -> Identifier
          -> Expression e

  Application :: ( Operation op, Eval op
                 , Arity op ~ n, V.Arity n
                 , Domain op e)
              => op
              -> Vec n (Expression e)
              -> Expression e

instance (Show e) => Show (Expression e) where
  show (Literal l)         = "(L " <> show l <> ")"
  show (Symbol s i)        = "(S " <> s <> "_" <> show i <> ")"
  show (Application op ev) = "(A " <> show op <> " " <> (foldMap show ev) <> ")"

plus :: forall t. Additive t => t -> t -> Expression t
plus x y = Application Addition $ V.mk2 (Literal x) (Literal y)

times :: forall t. Multiplicative t => t -> t -> Expression t
times x y = Application Multiplication $ V.mk2 (Literal x) (Literal y)
