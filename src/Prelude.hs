{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Prelude
  ( module Prelude

  , module Data.Proxy
  , module ClassyPrelude
  , Ratio
  , Natural
  , Unit

  , printf
  ) where

import           ClassyPrelude
import           Data.Proxy
import           Data.Ratio    (Ratio)
import           GHC.Natural   (Natural (..))
import           GHC.Tuple     (Unit)
import           Text.Printf   (printf)
