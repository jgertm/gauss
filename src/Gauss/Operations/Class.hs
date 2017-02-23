{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Gauss.Operations.Class
       ( module Gauss.Operations.Class
       ) where

import           ClassyPrelude

import           GHC.Exts       (Constraint)


type family Lift e args = liftedArgs
                        | liftedArgs -> args where
  Lift e (a,b)   = (e a, e b)
  Lift e (a,b,c) = (e a, e b, e c)

type family Lower e args where
  Lower e (e a, e b)      = (a, b)
  Lower e (e a, e b, e c) = (a, b, c)

class Operation op where
  type Domain op :: * -> * -> Constraint
  name :: op -> String

class (Operation op) => Eval op where
  evaluate :: (Domain op args codom)
           => pr op -> args -> codom
