module Gauss.Operations.Addition where

import           Gauss.Operations.Class
import qualified Gauss.Types.Int        as Int
import qualified Gauss.Types.Integer    as Integer

import           ClassyPrelude


data Addition = Addition
              deriving (Show, Eq)

instance Operation Addition (Int,Int) where
  type Codomain Addition (Int,Int) = Int
  evaluate _ = uncurry Int.add

instance Operation Addition (Integer,Integer) where
  type Codomain Addition (Integer,Integer) = Integer
  evaluate _ = uncurry Integer.add
