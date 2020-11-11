module Prelude.Time
  ( secondsToNominalDiffTime
  , module Data.Time
  )
where

import           Protolude
import           Data.Time
import           Data.Fixed                     ( Pico )

-- | Convert a value in seconds (with pico precision) to the nominal diff time format
secondsToNominalDiffTime :: Pico -> NominalDiffTime
secondsToNominalDiffTime = fromRational . toRational

