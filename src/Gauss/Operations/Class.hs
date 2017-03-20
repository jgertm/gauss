{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Gauss.Operations.Class
       ( module Gauss.Operations.Class
       ) where

import           ClassyPrelude


type family Lift e a where
  Lift e (a,b)   = (e a, e b)
  Lift e (a,b,c) = (e a, e b, e c)

class (Show op) => Operation op args where
  type family Codomain op args = cod | cod -> args

  evaluate :: op -> args -> Codomain op args
