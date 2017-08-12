{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gauss.Operations
       ( Operation(..), Lift

       , Addition(..)
       , Multiplication(..)

       , Monoid, Ring
       ) where


import           Prelude                         hiding (Monoid)

import           Gauss.Operations.Class
import           Gauss.Operations.Addition
import           Gauss.Operations.Multiplication


type Monoid (op :: *) (t :: *) = (Eq t, Operation op (t,t), Codomain op (t,t) ~ t)

type Ring (t :: *) = (Monoid Addition t, Monoid Multiplication t)
