{-# LANGUAGE
    TemplateHaskell
  , BlockArguments 
  , TypeOperators
  , DataKinds
  , PolyKinds
  , ConstraintKinds
  , TypeApplications
  , DefaultSignatures
  , DerivingVia 
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

  -- * Logging errors
  , logErrors

  -- * Handy re-exports.
  , module HTTP
  , module Log
  , module E

  -- $servant
  , asServantError
  , ServerError(..)
  ) where

import           Data.Aeson
import qualified Data.Text                     as T
import qualified GHC.Show
import           Network.HTTP.Types            as HTTP
import           Polysemy
import           Polysemy.Error                as E
import           Prelude.Control.Log           as Log
import           Protolude               hiding ( Reader )
import "servant-server" Servant.Server          ( ServerError(..) )

newtype ErrCode = ErrCode Text
                deriving (Eq, Show, IsString, Semigroup, Monoid, ToJSON, FromJSON) via Text

internalErrCode :: ErrCode
internalErrCode = "ERR.INTERNAL"

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
  displayKnownError e =  show e <> " + KnownError data: " <> show (errCode e, userMessage e, httpStatus e)

  -- | The logging level for the error.
  errorLogLevel :: e -> Log.Level

  -- | Wrap in KnownError
  knownError :: e -> KnownError
  knownError = KnownError

instance IsKnownError KnownError where
  errCode (KnownError e)   = errCode e
  errCode KnownException{} = internalErrCode

  httpStatus (KnownError e)   = httpStatus e
  httpStatus KnownException{} = internalServerError500

  errorLogLevel (KnownError e)   = errorLogLevel e
  errorLogLevel KnownException{} = Log.levelCritical

  knownError = identity

  userMessage (KnownError e)   = userMessage e
  userMessage KnownException{} = Just "An internal server error has occurred."

-- | Catch all `KnownError`s in Sem, log them, and throw them again. 
logErrors :: Members '[Logging , Error KnownError] r => Sem r a -> Sem r a
logErrors sem = sem `E.catch` (\err -> logKnownError err >> E.throw err)

logKnownError :: Member Logging r => KnownError -> Sem r ()
logKnownError err | level == Log.levelDebug    = Log.debug disp
                  | level == Log.levelInfo     = Log.info disp
                  | level == Log.levelWarning  = Log.warn disp
                  | level == Log.levelCritical = Log.critical disp
                  | otherwise                  = Log.error disp
 where
  level = errorLogLevel err
  disp  = displayKnownError err

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

