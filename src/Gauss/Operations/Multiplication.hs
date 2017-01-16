{-# LANGUAGE TypeFamilies    #-}

module Gauss.Operations.Multiplication where

import           Gauss.Operations.Class
import qualified Gauss.Types.Int        as Int
import qualified Gauss.Types.Integer    as Integer


data Multiplication = Multiplication deriving (Show, Eq, Ord)

instance Operation Multiplication where
  type Arity Multiplication = N2
  type Domain Multiplication = Multiplicative

class Multiplicative n where
  multiply :: n -> n -> n

instance Multiplicative Int where
  multiply = Int.multiply

instance Multiplicative Integer where
  multiply = Integer.multiply

instance Eval Multiplication where
  evaluate _ vn = inspect vn $ Fun multiply
