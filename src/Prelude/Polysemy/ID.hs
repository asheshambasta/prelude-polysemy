{-# LANGUAGE
    TemplateHaskell
  , BlockArguments
  , StrictData
  , TypeApplications
  , TypeOperators
  , DataKinds
  , PolyKinds
#-}
module Prelude.Polysemy.ID
  ( module Data.Snowflake
  , newIDGen
  , runIDGenIO
  , IDGen(..)
  , newSnowflake
  , newId
  )
where

import           Polysemy
import           Polysemy.Reader
import           Prelude
import           Data.Snowflake

data IDGen = IDGen
  { _idgSnowflakeConfig :: SnowflakeConfig
  , _idgSnowflakeGen    :: SnowflakeGen
  }

newIDGen :: SnowflakeConfig -> Integer -> IO IDGen
newIDGen conf node = IDGen conf <$> newSnowflakeGen conf node

data ID m a where
  NewId ::Integral i => ID m i
  NewSnowflake ::ID m Snowflake

makeSem ''ID

runIDGenIO
  :: Members '[Embed IO , Reader IDGen] r => Sem (ID ': r) a -> Sem r a
runIDGenIO = interpret $ \case
  NewSnowflake -> do
    IDGen { _idgSnowflakeGen = gen } <- ask
    embed $ nextSnowflake gen
  NewId -> fromIntegral . snowflakeToInteger <$> runIDGenIO newSnowflake

