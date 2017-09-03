module Gauss.Operations.Division where

import           Gauss.Operations.Class
import qualified Gauss.Types.Int        as Int
import qualified Gauss.Types.Integer    as Integer
import qualified Gauss.Types.Natural    as Natural

import           Data.Ratio             (Ratio)
import           Numeric.Natural        (Natural)


data Division = Division
              deriving (Show, Eq)

instance Operation Division (Int,Int) where
  type Codomain Division (Int,Int) = Ratio Int
  evaluate _ = uncurry Int.divide

instance Operation Division (Integer,Integer) where
  type Codomain Division (Integer,Integer) = Ratio Integer
  evaluate _ = uncurry Integer.divide

instance Operation Division (Natural,Natural) where
  type Codomain Division (Natural,Natural) = Ratio Natural
  evaluate _ = uncurry Natural.divide
