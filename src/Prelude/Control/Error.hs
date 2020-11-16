{-# LANGUAGE
    DefaultSignatures
  , DerivingVia
  , ConstraintKinds
#-}
module Prelude.Control.Error
  ( ErrCode(..)
  , internalErrCode
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

  -- $servant
  , asServantError
  , ServerError(..)
  )
where

import           Control.Monad.Log             as MLog
import           Data.Aeson
import "servant-server" Servant.Server          ( ServerError(..) )
import qualified Data.Text                     as T
import qualified GHC.Show
import           Polysemy.Error                as E
import           Polysemy
import           Prelude.Control.Log           as Log
import           Network.HTTP.Types            as HTTP
import           Protolude

newtype ErrCode = ErrCode Text
                deriving (Eq, Show, IsString, Semigroup, Monoid, ToJSON, FromJSON) via Text

internalErrCode :: ErrCode
internalErrCode = ErrCode "ERR.INTERNAL"

data KnownError where
  KnownError ::IsKnownError e => e -> KnownError
  KnownException ::Exception e => e -> KnownError

instance Show KnownError where
  show = \case
    KnownError     e -> T.unpack $ "KnownError " <> displayKnownError e
    KnownException e -> "KnownException " <> show e

instance Exception KnownError

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

instance IsKnownError KnownError where
  errCode (KnownError     e) = errCode e
  errCode (KnownException e) = internalErrCode

  httpStatus (KnownError e)   = httpStatus e
  httpStatus KnownException{} = internalServerError500

  errorLogLevel (KnownError e)   = errorLogLevel e
  errorLogLevel KnownException{} = MLog.levelCritical

  knownError = identity

  userMessage (KnownError e)   = userMessage e
  userMessage KnownException{} = Just "An internal server error has occurred."

-- | Throw an error.
throwKnownError :: (IsKnownError e, KnownErrors r) => e -> Sem r a
throwKnownError = E.throw . knownError

instance ToJSON KnownError where
  toJSON e = object ["errCode" .= errCode e, "userMessage" .= userMessage e]

-- $constraints
-- Handy constriants for less typing.

type KnownErrors r = Member (E.Error KnownError) r

-- $servant
-- Useful for wiring up custom monads (ExceptT KnownError) etc. to Servant's Handlers.

-- | Converting to Servant's `ServerError`.
asServantError :: IsKnownError e => e -> ServerError
asServantError e = ServerError
  { errHTTPCode     = statusCode httpStatus'
  , errReasonPhrase = statusMessage'
  , errBody         = encode (knownError e)
  , errHeaders      = [(hContentType, "application/json;charset=utf-8")]
  }
 where
  httpStatus'    = httpStatus e
  statusMessage' = T.unpack . decodeUtf8 $ statusMessage httpStatus'

