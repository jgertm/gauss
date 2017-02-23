{-# LANGUAGE FunctionalDependencies #-}

module Gauss.Operations.Multiplication where

import           Gauss.Operations.Class
import qualified Gauss.Types.Int        as Int
import qualified Gauss.Types.Integer    as Integer

import ClassyPrelude


data Multiplication = Multiplication deriving (Show, Eq)

instance Operation Multiplication where
  type Domain Multiplication = Multiplicative
  name _ = "Multiplication"

instance Eval Multiplication where
  evaluate _ = multiply

class Multiplicative domain codomain | domain -> codomain where
  multiply :: domain -> codomain

instance Multiplicative (Int,Int) Int where
  multiply = uncurry Int.multiply

instance Multiplicative (Integer,Integer) Integer where
  multiply = uncurry Integer.multiply
