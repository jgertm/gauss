module Gauss.Types.Int where

import           Data.Ratio ((%))


add, multiply, subtract :: Int -> Int -> Int
add      = (+)
multiply = (*)
subtract = (-)

divide :: Int -> Int -> Ratio Int
divide = (%)
