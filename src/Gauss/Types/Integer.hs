module Gauss.Types.Integer where

import           Data.Ratio ((%))
import           GHC.Integer


add, multiply, subtract :: Integer -> Integer -> Integer
add      = plusInteger
multiply = timesInteger
subtract = minusInteger

divide :: Integer -> Integer -> Ratio Integer
divide = (%)
