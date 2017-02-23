module Gauss.Types.Integer where

import           Data.Dynamic
import           GHC.Integer


rep :: TypeRep
rep = typeRep (Proxy :: Proxy Integer)

add, multiply :: Integer -> Integer -> Integer
add      = plusInteger
multiply = timesInteger
