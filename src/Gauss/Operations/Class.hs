{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Gauss.Operations.Class where

import           Gauss.Argument

class (Show op, Argument args)
     => Operation op args where
  type Codomain op args :: *
  evaluate :: op -> args -> Codomain op args
