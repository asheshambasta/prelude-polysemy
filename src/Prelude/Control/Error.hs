{-# LANGUAGE
    DefaultSignatures
  , DerivingVia
  , ConstraintKinds
#-}
module Prelude.Control.Error
  ( ErrCode(..)
  , KnownError(..)
  , IsKnownError(..)

  -- $constriants
  , KnownErrors 

  -- * Throwing errors.
  , throwKnownError

  -- * Handy re-exports.
  , module HTTP
  , module Log
  , module E
  )
where

import qualified Data.Text                     as T
import qualified GHC.Show
import           Polysemy.Error                as E
import           Polysemy
import           Prelude.Control.Log                   as Log
import           Network.HTTP.Types            as HTTP
import           Protolude

newtype ErrCode = ErrCode Text
                deriving (Eq, Show, IsString, Semigroup, Monoid) via Text

data KnownError where
  KnownError ::IsKnownError e => e -> KnownError
  KnownException ::Exception e => e -> KnownError

instance Show KnownError where
  show = \case
    KnownError  e -> T.unpack $ "KnownError " <> displayKnownError e
    KnownException e -> "KnownException " <> show e

-- | An instance of `IsKnownError` indicates if a given type can be serialised to a general error representation.
class IsKnownError e where

  -- | Error code, done for api documentation etc.
  errCode :: e -> ErrCode

  -- | Human friendly error message
  userMessage :: e -> Maybe Text

  -- | HTTP Status 
  httpStatus :: e -> HTTP.Status

  -- | Show the error.
  displayKnownError :: e -> Text

  default displayKnownError :: Show e => e -> Text
  displayKnownError e =  show e <> ": " <> show (errCode e, userMessage e, httpStatus e)

  -- | The logging level for the error.
  errorLogLevel :: e -> Log.Level

  -- | Wrap in KnownError
  knownError :: e -> KnownError
  knownError = KnownError

-- | Throw an error.
throwKnownError :: (IsKnownError e, KnownErrors r) => e -> Sem r a
throwKnownError = E.throw . knownError

-- $constraints
-- Handy constriants for less typing.

type KnownErrors r = Member (E.Error KnownError) r
