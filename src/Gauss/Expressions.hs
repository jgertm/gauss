{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gauss.Expressions where

import           Gauss.Operations

import           Data.Monoid
import qualified Data.Vector.Fixed       as V
import           Data.Vector.Fixed.Boxed


type Identifier = Int

data Expression dom where
  Literal :: dom
          -> Expression dom

  Symbol  :: String
          -> Identifier
          -> Expression dom

  Application :: ( Operation op, Eval op
                 , Arity op ~ n, V.Arity n
                 , Domain op dom cod
                 , Show dom)
              => op
              -> Vec n (Expression dom)
              -> Expression cod

instance (Show dom) => Show (Expression dom) where
  show (Literal l)         = "(L " <> show l <> ")"
  show (Symbol s i)        = "(S " <> s <> "_" <> show i <> ")"
  show (Application op ev) = "(A " <> show op <> " " <> (foldMap show ev) <> ")"

plus :: (Additive dom cod) => dom -> dom -> Expression cod
plus x y = Application Addition $ V.mk2 (Literal x) (Literal y)

times :: (Multiplicative dom cod) => dom -> dom -> Expression cod
times x y = Application Multiplication $ V.mk2 (Literal x) (Literal y)
