module Gauss.Operations.Addition where

import           Gauss.Operations.Class
import qualified Gauss.Types.Int        as Int
import qualified Gauss.Types.Integer    as Integer
import           Gauss.Types.Natural    (Natural)
import qualified Gauss.Types.Natural    as Natural


data Addition = Addition
              deriving (Show, Eq)

instance Operation Addition (Int,Int) where
  type Codomain Addition (Int,Int) = Int
  evaluate _ = uncurry Int.add

instance Operation Addition (Integer,Integer) where
  type Codomain Addition (Integer,Integer) = Integer
  evaluate _ = uncurry Integer.add

instance Operation Addition (Natural, Natural) where
  type Codomain Addition (Natural,Natural) = Natural
  evaluate _ = uncurry Natural.add
