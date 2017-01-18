{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Gauss.Operations.Addition where

import           Gauss.Operations.Class
import qualified Gauss.Types.Int        as Int
import qualified Gauss.Types.Integer    as Integer


data Addition = Addition deriving (Show, Eq)

instance Operation Addition where
  type Arity Addition = N2
  type Domain Addition = Additive

class (Show domain) => Additive domain codomain | domain -> codomain where
  add :: domain -> domain -> codomain

instance Additive Int Int where
  add = Int.add

instance Additive Integer Integer where
  add = Integer.add

instance Eval Addition where
  evaluate _ vn = inspect vn $ Fun add
