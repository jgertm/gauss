{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Gauss.Operations.Class where


class ( Show op )
     => Operation op args where
  type family Codomain op args :: *
  evaluate :: op -> args -> Codomain op args
