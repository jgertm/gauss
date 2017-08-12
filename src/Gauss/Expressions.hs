module Gauss.Expressions where

import           Gauss.Expressions.Input     as I
import           Gauss.Expressions.Reduction as R
import           Gauss.Operations

import           ClassyPrelude

import           Data.Dynamic


abstract :: I.Expression t -> R.Expression
abstract (I.Literal l)           = R.Literal $ toDyn l
abstract (I.Symbol s)            = R.Symbol s
abstract (I.Application op args) = R.Application op $ I.cata abstract args
