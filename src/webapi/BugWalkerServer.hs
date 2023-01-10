{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main module of the BugWalker WebAPI Server
module BugWalkerServer
    (
      runServerWithJWT,
      server
    ) where
---
import Data.Time
import Network.Wai ( Middleware, Application )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Cors
    ( cors,
      simpleCorsResourcePolicy,
      CorsResourcePolicy(corsMethods, corsRequestHeaders) )
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant
    ( Proxy(..),
      serveWithContext,
      err401,
      type (:<|>)(..),
      Capture,
      JSON,
      QueryParam,
      ReqBody,
      type (:>),
      Delete,
      Get,
      Post,
      Put,
      Server,
      Context((:.), EmptyContext) )
import Servant.Auth.Server
    ( defaultCookieSettings,
      defaultJWTSettings,
      Auth,
      JWT,
      CookieSettings,
      JWTSettings,
      ThrowAll(throwAll),
      AuthResult(Authenticated) )
import Control.Monad (when)
import Data.Int ( Int64 )
import Data.ByteString (ByteString)
---
import ApiTypes
    ( Bug,
      BugChangedResponseMessage,
      BugDetail,
      BugStats,
      BuildT,
      Comment,
      LoginResponseMessage,
      ResponseMessage,
      SpellT,
      UserChangedResponseMessage )
import AuthTypes
    ( AuthenticatedUser,
      UserChangeRoleData,
      UserL,
      UserLoginData,
      UserP,
      UserRegistration )
import CommonApi ( versionHandler )
import AssetApi
    ( iohInitHandler, listBuildsHandler, listSpellsHandler )
import BugApi
    ( deleteBugHandler,
      getBugHandler,
      getPendingBugsHandler,
      getBugsByUserHandler,
      getBugsHandler,
      postBugHandler,
      putBugHandler,
      getBugStatsHandler)
import UserApi
    ( userRegistrationHandler,
      userLoginHandler,
      getUsersHandler,
      getUserPHandler,
      changeUserRoleHandler,
      getUserSecretHandler)
import CommentApi ( getCommentsHandler, postCommentHandler, deleteCommentHandler, getLatestCommentsHandler )
import JwkStore (loadJwk)
import Logging
import Queue
---
{- API -}

-- | The unprotected (no auth) HTTP endpoints in the API
type Unprotected =
  "api" :> "version" :> Get '[JSON] ResponseMessage
  :<|> "api" :> "builds" :> Get '[JSON] [BuildT]
  :<|> "api" :> "spells" :> Get '[JSON] [SpellT]
  :<|> "api" :> "bugs" :> QueryParam "limit" Int :> Get '[JSON] [Bug]
  :<|> "api" :> "bugs" :> Capture "bugId" Int64 :> Get '[JSON] BugDetail
  :<|> "api" :> "bugs" :> "user" :> Capture "uId" Int64 :> Get '[JSON] [Bug]
  :<|> "api" :> "bugs" :> "stats" :> Get '[JSON] BugStats
  -- users
  :<|> "api" :> "users" :> "register" :> ReqBody '[JSON] AuthTypes.UserRegistration :> Post '[JSON] UserChangedResponseMessage
  :<|> "api" :> "users" :> "login" :> ReqBody '[JSON] AuthTypes.UserLoginData :> Post '[JSON] LoginResponseMessage
  :<|> "api" :> "users" :> Get '[JSON] [AuthTypes.UserL]
  :<|> "api" :> "user" :> Capture "uId" Int64 :> Get '[JSON] AuthTypes.UserP
  -- comments
  :<|> "api" :> "comments" :> Capture "bugId" Int64 :> Get '[JSON] [Comment]
  :<|> "api" :> "comments" :> "latest" :> Get '[JSON] [Comment]

-- | The protected (auth) HTTP endpoints in the API
type Protected =
  "api" :> "secret" :> Get '[JSON] AuthenticatedUser
  :<|> "api" :> "bugs" :> ReqBody '[JSON] BugDetail :> Post '[JSON] BugChangedResponseMessage
  :<|> "api" :> "bugs" :> Capture "bugId" Int64 :> ReqBody '[JSON] BugDetail :> Put '[JSON] BugChangedResponseMessage
  :<|> "api" :> "bugs" :> Capture "bugId" Int64 :> Delete '[JSON] BugChangedResponseMessage
  :<|> "api" :> "purps" :> "bugs" :> "pending" :> Get '[JSON] [Bug]
  :<|> "api" :> "ioh" :> "init" :> Capture "source" String :> Get '[JSON] ResponseMessage
  :<|> "api" :> "user" :> "role" :> ReqBody '[JSON] AuthTypes.UserChangeRoleData :> Post '[JSON] UserChangedResponseMessage
  :<|> "api" :> "comments" :> ReqBody '[JSON] Comment :> Post '[JSON] ResponseMessage
  :<|> "api" :> "comments" :> Capture "cId" Int64 :> Delete '[JSON] ResponseMessage
-- | Generates the protected Server by configuring the AuthenticatedUser
-- type as the user auth context
protected :: Logger -- ^ the logger context
          -> Servant.Auth.Server.AuthResult AuthenticatedUser -- ^ the auth result context
          -> Server Protected -- ^ The server for our protected endpoints
protected lgr (Servant.Auth.Server.Authenticated user) =
  getUserSecretHandler user
  :<|> postBugHandler lgr user
  :<|> putBugHandler lgr user
  :<|> deleteBugHandler lgr user
  :<|> getPendingBugsHandler (Just 10000) user
  :<|> iohInitHandler user
  :<|> UserApi.changeUserRoleHandler user
  :<|> CommentApi.postCommentHandler user
  :<|> CommentApi.deleteCommentHandler user
protected _ _ = throwAll err401

-- | Generates the unprotected Server
unprotected :: Logger 
            -> CookieSettings -- ^ The configured cookie settings
            -> JWTSettings -- ^ the configured JWTSettings
            -> Server Unprotected -- ^ The server for our unprotected endpoints
unprotected lgr cs jwts =
  versionHandler
  :<|> listBuildsHandler
  :<|> listSpellsHandler lgr
  :<|> getBugsHandler
  :<|> getBugHandler lgr
  :<|> getBugsByUserHandler
  :<|> getBugStatsHandler
  :<|> UserApi.userRegistrationHandler
  :<|> UserApi.userLoginHandler jwts
  :<|> UserApi.getUsersHandler
  :<|> UserApi.getUserPHandler
  :<|> CommentApi.getCommentsHandler
  :<|> CommentApi.getLatestCommentsHandler

{- Server -}

type API auths = (Servant.Auth.Server.Auth auths AuthenticatedUser :> Protected) :<|> Unprotected

-- | Creates the full server by combining the unprotcted and protected
-- servers and their auth contexts
server :: Logger -> CookieSettings -> JWTSettings -> Server (API auths)
server logger cs jwts = protected logger :<|> unprotected logger cs jwts

-- | Middleware for configuring CORS on the WAI server
allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

-- | The CORS policy to use in the CORS WAI middleware
appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }

-- | Exports the Wai Application
-- ** Exclusively for testing the API - don't use this in production **
getTestApplication :: Int -> Network.Wai.Application
getTestApplication port = undefined


-- | The main "run" function of the BugWalker API Server.
-- Attempts to run the full WAI server using JWT auth
runServerWithJWT :: Logging.Logger -- ^ The logger to use
                 -> Int -- ^ The port to run the server on
                 -> IO ()
runServerWithJWT log port = do
  Logging.logInfo log "BWS" "Starting server..."
  (isLocal,jwk) <- loadJwk "data/jwk"
  if isLocal then
    Logging.logDebug log "BWS" "Using local JWK"
  else
    Logging.logDebug log "BWS" "Generating new Jwk..."
  Logging.logDebug log "BWS" "Loaded JWK, generating proxy ..."
  let jwtCfg = defaultJWTSettings jwk
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])
  Logging.logDebug log "BWS" "Init Complete. Running WebServer..."
  run port $ allowCors $ serveWithContext api cfg (server log defaultCookieSettings jwtCfg)
