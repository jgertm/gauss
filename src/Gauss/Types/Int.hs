{-# LANGUAGE NoImplicitPrelude #-}

module Gauss.Types.Int where

import           Gauss.Operations

import           ClassyPrelude

import           Data.Dynamic


rep :: TypeRep
rep = typeRep (Proxy :: Proxy Int)

add, multiply :: Int -> Int -> Int
add      = (+)
multiply = (*)

operations :: OperationMap
operations = fmap toDyn . mapFromList $
  [ (Addition, add)
  , (Multiplication, multiply)]
