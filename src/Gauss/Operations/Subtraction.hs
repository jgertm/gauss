module Gauss.Operations.Subtraction where

import           Gauss.Operations.Class
import qualified Gauss.Types.Int        as Int
import qualified Gauss.Types.Integer    as Integer
import qualified Gauss.Types.Natural    as Natural


data Subtraction = Subtraction
                 deriving (Show, Eq)

instance Operation Subtraction (Int,Int) where
  type Codomain Subtraction (Int,Int) = Int
  evaluate _ = uncurry Int.subtract

instance Operation Subtraction (Integer,Integer) where
  type Codomain Subtraction (Integer,Integer) = Integer
  evaluate _ = uncurry Integer.subtract

instance Operation Subtraction (Natural,Natural) where
  type Codomain Subtraction (Natural,Natural) = Integer
  evaluate _ = uncurry Natural.subtract
