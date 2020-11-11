module Prelude
  ( module Protolude
  , module Err
  , module Log
  , module HTTP
  -- * Text
  , pack
  , unpack
  , pShow
  )
where

import           Data.Text.Lazy                 ( toStrict )
import qualified Text.Pretty.Simple            as PS
import           Network.HTTP.Types            as HTTP
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Protolude               hiding ( catch
                                                , Reader
                                                , ask
                                                )
import           Prelude.Control.Error         as Err
import           Prelude.Control.Log           as Log

pShow :: Show a => a -> Text
pShow = toStrict . PS.pShow
