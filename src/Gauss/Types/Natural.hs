module Gauss.Types.Natural
       ( module Gauss.Types.Natural
       , Natural
       ) where

import           Gauss.Types.Integer as Integer

import           Data.Ratio          (Ratio, (%))
import           Numeric.Natural     (Natural)


add, multiply :: Natural -> Natural -> Natural
add      = (+)
multiply = (*)

subtract :: Natural -> Natural -> Integer
subtract m n = Integer.subtract (fromIntegral m) (fromIntegral n)

divide :: Natural -> Natural -> Ratio Natural
divide = (%)
