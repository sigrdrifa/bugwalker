{-# LANGUAGE OverloadedStrings #-}

-- | Handlers related to Bug API endpoints
module BugApi
    (
      getBugsHandler,
      getBugsByUserHandler,
      getPendingBugsHandler,
      getBugHandler,
      postBugHandler,
      putBugHandler,
      deleteBugHandler,
      getBugStatsHandler
    ) where
---
import Servant
    ( throwError,
      Handler,
      err400,
      err401,
      err404,
      err500,
      ServerError(errBody) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.ByteString (pack)
import Control.Monad (when, unless)
import Data.ByteString.Lazy.Internal (packChars)
import Data.Int ( Int64 )
import Data.Maybe (fromMaybe)
---
import AuthTypes ( isUserInit, isUserNovice, isUserMod, AuthenticatedUser(auId) )
import IOH
    ( changeBug,
      getBug,
      getBugs,
      getPendingBugs,
      getBugsByUser,
      removeBug,
      storeBug,
      getBugStats,
      IOHError(IOHFetchError, IOHAuthError) )
import ApiTypes
    ( Bug,
      BugChangedResponseMessage(BugChangedResponseMessage),
      BugDetail(bugdtSubmitter),
      BugStats) 
import Logging (Logger)
----

-- | Handles fetching bugs
getBugsHandler :: Maybe Int -- ^ Captured limit of how many bugs to get
               -> Handler [Bug]
getBugsHandler limit = case limit of
  Just l -> liftIO $ IOH.getBugs l
  Nothing -> liftIO $ IOH.getBugs 200 -- Default to 200

-- | Handles fetching bug statistics
getBugStatsHandler :: Handler BugStats
getBugStatsHandler = do 
  res <- liftIO $ IOH.getBugStats False
  case res of
    Right bs -> return bs
    _        -> Servant.throwError err500

-- | Handles fetching bugs
getPendingBugsHandler :: Maybe Int -- ^ Captured limit of how many bugs to get
               -> AuthenticatedUser -- ^ the requesting user
               -> Handler [Bug]
getPendingBugsHandler limit au = do
    unless (isUserMod au)
      (Servant.throwError err401 {errBody = "User must be Mod role or higher"})
    let l = fromMaybe 10000 limit
    bugs <- liftIO $ IOH.getPendingBugs l au
    case bugs of
      Right b -> return b
      Left (IOHAuthError m)  -> Servant.throwError err400 { errBody = packChars m }
      Left (IOHFetchError m) -> Servant.throwError err404 { errBody = packChars m }
      _                      -> Servant.throwError err500 { errBody = "Failed to update bug"}


-- | Handles fetching bugs by userId
getBugsByUserHandler :: Int64 -- ^ unique userId to fetch bugs submitted by
                     -> Handler [Bug]
getBugsByUserHandler uId = liftIO $ IOH.getBugsByUser uId

-- | Handles fetching a single bug
getBugHandler :: Logger -- ^ the logging context
              -> Int64 -- ^ unique bugId to fetch
              -> Handler BugDetail
getBugHandler lgr x = do
  b <- liftIO $ IOH.getBug lgr x
  case b of
    Just r -> return r
    Nothing -> Servant.throwError err404 {
      errBody = "Four-Oh-Four! The requested bug was not found." }

-- | (Init+ Only) Handles fsubmitting a new request
postBugHandler :: Logger -- ^ the logging context
               -> AuthenticatedUser -- ^ the requesting user (Init+)
               -> BugDetail -- ^ the bug data to insert
               -> Handler BugChangedResponseMessage
postBugHandler lgr au bd = do
  when (auId au /= bugdtSubmitter bd)
    (Servant.throwError err400 {errBody = "UserID mismatch on Bug"})
  unless (isUserNovice au)
    (Servant.throwError err401 {errBody = "User must be Novice role or higher"})
  r <- liftIO $ IOH.storeBug lgr au bd
  case r of
    Just id ->
      return $ BugChangedResponseMessage 200 id "OK" $
               "Inserted bug with ID " ++ show id
    Nothing -> Servant.throwError err500

-- | (Init+ only) Attempts to update a bug with the provided data
putBugHandler :: Logger -- ^ the logging context
              -> AuthenticatedUser -- ^ the requesting user (init+)
              -> Int64 -- ^ the unique id of the bug to update
              -> BugDetail -- ^ updated bug data to insert
              -> Handler BugChangedResponseMessage
putBugHandler lgr au id bd = do
  when (auId au /= bugdtSubmitter bd)
    (Servant.throwError err400 {errBody = "UserID mismatch on Bug"})
  unless (isUserNovice au)
    (Servant.throwError err401 {errBody = "User must be Novice role or higher"})
  r <- liftIO $ IOH.changeBug lgr au id bd
  case r of
      Right dbId ->
        return $ BugChangedResponseMessage 200 dbId "OK" $ "Updated bug with ID " ++ show dbId
      Left (IOHAuthError m)  -> Servant.throwError err400 { errBody = packChars m }
      Left (IOHFetchError m) -> Servant.throwError err404 { errBody = packChars m }
      _                      -> Servant.throwError err500 { errBody = "Failed to update bug"}

-- | (Init+ Only) Handles deleting a single bug 
deleteBugHandler :: Logger -- ^ the logging context
                 -> AuthenticatedUser -- ^ the requesting user (init+)
                 -> Int64 -- ^ the unique id of the bug to delete
                 -> Handler BugChangedResponseMessage
deleteBugHandler lgr au bId = do
  unless (isUserInit au)
    (Servant.throwError err401 {errBody = "User must be Initiate role or higher"})
  r <- liftIO $ IOH.removeBug lgr au bId
  case r of
      Right True             -> pure $ BugChangedResponseMessage 200 bId "OK" $ show bId
      Left (IOHAuthError m)  -> Servant.throwError err400 { errBody = packChars m }
      Left (IOHFetchError m) -> Servant.throwError err404 { errBody = packChars m }
      _                      -> Servant.throwError err500 { errBody = "Failed to delete bug"}
