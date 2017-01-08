{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gauss.Types.Integer where

import           Gauss.Operations

import           ClassyPrelude

import           Data.Dynamic
import           GHC.Integer


rep :: TypeRep
rep = typeRep (Proxy :: Proxy Integer)

add, multiply :: Integer -> Integer -> Integer
add      = plusInteger
multiply = timesInteger

operations :: OperationMap
operations = fmap toDyn . mapFromList $
  [ (Addition,       add)
  , (Multiplication, multiply) ]
