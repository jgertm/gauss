{-# LANGUAGE NoImplicitPrelude #-}

module Gauss.Operations where

import           ClassyPrelude

import           Data.Dynamic

data Operation = Addition
               | Multiplication
               deriving (Show, Eq, Ord)

type OperationMap = Map Operation Dynamic
