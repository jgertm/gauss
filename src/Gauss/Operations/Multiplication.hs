{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Gauss.Operations.Multiplication where

import           Gauss.Operations.Class
import qualified Gauss.Types.Int        as Int
import qualified Gauss.Types.Integer    as Integer


data Multiplication = Multiplication deriving (Show, Eq)

instance Operation Multiplication where
  type Arity Multiplication = N2
  type Domain Multiplication = Multiplicative

class (Show domain) => Multiplicative domain codomain | domain -> codomain where
  multiply :: domain -> domain -> codomain

instance Multiplicative Int Int where
  multiply = Int.multiply

instance Multiplicative Integer Integer where
  multiply = Integer.multiply

instance Eval Multiplication where
  evaluate _ vn = inspect vn $ Fun multiply
