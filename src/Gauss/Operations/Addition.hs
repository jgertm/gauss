{-# LANGUAGE TypeFamilies #-}

module Gauss.Operations.Addition where

import           Gauss.Operations.Class
import qualified Gauss.Types.Int        as Int
import qualified Gauss.Types.Integer    as Integer


data Addition = Addition deriving (Show, Eq, Ord)

instance Operation Addition where
  type Arity Addition = N2
  type Domain Addition = Additive

class Additive n where
  add :: n -> n -> n

instance Additive Int where
  add = Int.add

instance Additive Integer where
  add = Integer.add

instance Eval Addition where
  evaluate _ vn = inspect vn $ Fun add
