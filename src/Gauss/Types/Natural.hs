module Gauss.Types.Natural where

import           Gauss.Types.Integer as Integer

import           Data.Ratio ((%))


add, multiply :: Natural -> Natural -> Natural
add      = (+)
multiply = (*)

subtract :: Natural -> Natural -> Integer
subtract m n = Integer.subtract (fromIntegral m) (fromIntegral n)

divide :: Natural -> Natural -> Ratio Natural
divide = (%)
