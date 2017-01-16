module Gauss.Types.Int where

import           Data.Dynamic


rep :: TypeRep
rep = typeRep (Proxy :: Proxy Int)

add, multiply :: Int -> Int -> Int
add      = (+)
multiply = (*)
