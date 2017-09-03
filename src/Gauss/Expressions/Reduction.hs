{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Gauss.Expressions.Reduction where

import qualified Gauss.Expressions.Input as EI
-- import           Gauss.Operations

import Data.Dynamic
import Data.Typeable

data Expression = Leaf (Either (TypeRep, EI.SymbolID)
                               Dynamic)
                | Node [Expression]
