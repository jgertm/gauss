{-# LANGUAGE FunctionalDependencies #-}

module Gauss.Operations.Addition where

import           Gauss.Operations.Class
import qualified Gauss.Types.Int        as Int
import qualified Gauss.Types.Integer    as Integer

import           ClassyPrelude


data Addition = Addition deriving (Show, Eq)

instance Operation Addition where
  type Domain Addition = Additive
  name _ = "Addition"

instance Eval Addition where
  evaluate _ = add

class Additive domain codomain | domain -> codomain where
  add :: domain -> codomain

instance Additive (Int, Int) Int where
  add = uncurry Int.add

instance Additive (Integer, Integer) Integer where
  add = uncurry Integer.add
