{-# LANGUAGE OverloadedStrings #-}

-- | IO Abstraction handler used to interact with an underlying
--  data store. All Datastore io should go through this.
module IOH
    ( initIO,
      storeSpellData,
      getBuilds,
      getSpells,
      storeBug,
      changeBug,
      getBug,
      removeBug,
      getBugs,
      getPendingBugs,
      getBugsByUser,
      storeUser,
      getUser,
      getUserByName,
      getUsers,
      getUserL,
      changeUserRole,
      storeComment,
      getComments,
      getBugStats,
      getLatestComments,
      IOH.deleteComment,
      IOHError (..)
    ) where
---
import Data.Int ( Int64 )
import Control.Exception ( catch, IOException )
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import qualified Data.Text as T (pack)
---
import SQLiteProvider
    ( createDb,
      deleteBug,
      insertBug,
      insertComment,
      insertSpellData,
      insertUser,
      selectBug,
      selectBugs',
      selectBugsByUser,
      selectBuilds,
      selectComment,
      selectComments,
      selectSpells,
      selectUser,
      selectUserByName,
      selectUserL,
      selectUsers,
      updateBug,
      updateUserRole,
      deleteComment,
      deleteCommentsByUser,
      selectBugStats,
      selectLatestComments)
import GameDataTypes ( SpellData )
import ApiTypes
    ( Bug (bugStatus),
      BugStatus (..),
      BugDetail(bugdtSeverity, bugdtType, bugdtTitle, bugdtSpellId,
                bugdtBuildId, bugdtSpec, bugdtTags, bugdtDescription, bugdtSteps,
                bugdtContent, bugdtBlueTrackerLink, bugdtSubmitter),
      BuildT(BuildT),
      Comment(cBody, cUserId),
      SpellT,
      BugStats (..))
import AuthTypes
    ( isUserAdmin,
      isUserInit,
      isUserMod,
      userAdminRole,
      AuthenticatedUser(auId),
      User(userRole),
      UserL )
import Logging (Logger, logError, logDebug, logInfo)
---

{- Data Types -}

-- | Represents differnet errors that can occur during IOH actions
data IOHError
    = IOHInsertError String
    | IOHFetchError String
    | IOHDeleteError String
    | IOHAuthError String
    | IOHUpdateError String
    deriving (Show)

{- Functions -}

-- | Calls on the underlying provider to init its datastore
initIO :: Bool -- ^ Do a hardInit
       -> IO ()
initIO isHardInit = do
    putStrLn "[IOH] Init started..."
    SQLiteProvider.createDb isHardInit
    putStrLn "[IOH] Init complete."

-- | Attempt to store a full SpellData collection (from a GameData update)
storeSpellData :: SpellData -- ^ the SpellData to store
               -> IO ()
storeSpellData sd = do
    putStrLn "[IOH] Inserting SpellData..."
    SQLiteProvider.insertSpellData sd
    putStrLn "[IOH] Done."

-- | Attempt to insert a Bug into the data store
storeBug :: Logger -- ^ Logging instance
         -> AuthenticatedUser -- ^ the requesting user
         -> ApiTypes.BugDetail -- ^ The bug to insert
         -> IO (Maybe Int64) -- ^ IO (id of the inserted bug or Nothing)
storeBug log au bd = do
    logDebug log "IOH" "Attempting to insert bug..."
    bId <- catch (SQLiteProvider.insertBug s t status ti ta sId buId sp de st ct btl u) (\e -> do
        logError log "IOH" $ T.pack $ "Failed to insert bug: " ++ show (e :: IOException)
        logError log "IOH" $ T.pack $ show bd
        pure Nothing)
    case bId of
        Just i -> do
            logDebug log "IOH" $ T.pack $ "Stored BugDetail with ID " ++ show i ++ "."
            pure $ Just i
        Nothing -> do
            logInfo log "IOH" "Failed to store bug."
            pure Nothing
    where
        s = fromEnum (bugdtSeverity bd)
        t = fromEnum (bugdtType bd)
        ti = bugdtTitle bd
        sId = bugdtSpellId bd
        buId = bugdtBuildId bd
        status = if isUserInit au then 1 else 4
        sp = fromEnum (bugdtSpec bd)
        [ta, de, st, ct, btl] =
            map (\f -> fromMaybe "" $ f bd) [bugdtTags, bugdtDescription, bugdtSteps,
            bugdtContent, bugdtBlueTrackerLink]
        u = bugdtSubmitter bd

-- | Attempt to fetch a bug from the data store
getBug :: Logger -- ^ the logging context
       -> Int64 -- ^ Unique Id of the bug to fetch
       -> IO (Maybe ApiTypes.BugDetail)
getBug lgr bid = do
    logDebug lgr "IOH" $ T.pack $ "Fetching bug with ID " ++ show bid
    SQLiteProvider.selectBug bid

-- | Attempt to update the stored data of a bug
changeBug :: Logger -- ^ the logging context
           -> AuthenticatedUser -- ^ the user making the request
           -> Int64 -- ^ the unique id of the bug to update
           -> ApiTypes.BugDetail -- ^ the modified bug detail data
           -> IO (Either IOHError Int64)
changeBug lgr au bugId bd = do
    b <- getBug lgr bugId
    case b of
        Just bug ->
            if isUserMod au || auId au == bugdtSubmitter bug then do
                r <- SQLiteProvider.updateBug bugId bd
                case r of
                    Just id -> pure $ Right id
                    Nothing -> pure $ Left $ IOHInsertError ("Failed to update bug with id " ++ show bugId)
            else pure $ Left $ IOHAuthError "Insufficient permissions"
        Nothing -> pure $ Left $ IOHFetchError "Requested bug not found"

-- | Attempt to delete a bug from the data store
removeBug :: Logger -- ^ the logging context
          -> AuthenticatedUser -- ^ The user making the request
          -> Int64 -- ^ the unique id of the bug to delete
          -> IO (Either IOHError Bool)
removeBug lgr au bugId = do
    b <- getBug lgr bugId
    case b of
        Just bug ->
            if isUserMod au || auId au == bugdtSubmitter bug then do
                r <- SQLiteProvider.deleteBug bugId
                if r then pure $ Right True else pure $ Left $ IOHDeleteError "Failed to delete bug"
            else pure $ Left $ IOHAuthError "Insufficient permissions"
        Nothing -> pure $ Left $ IOHFetchError "Requested bug not found"

-- | Fetches all the bugs from the data store
getBugs :: Int -- ^ Limit on how many bugs to fetch
        -> IO [ApiTypes.Bug]
getBugs l = do
    bugs <- SQLiteProvider.selectBugs' l
    pure $ filter (\x -> bugStatus x /= Pending) bugs


-- | Fetches all pending bugs (mod only) 
getPendingBugs :: Int -- ^ Limit on how many bugs to fetch
               -> AuthenticatedUser -- ^ The user making the request
               -> IO (Either IOHError [ApiTypes.Bug])
getPendingBugs l au = do
  if not (isUserMod au) then 
    pure $ Left $ IOHAuthError "Insufficient permissions"
  else do
    bugs <- SQLiteProvider.selectBugs' l
    pure $ Right $ filter (\x -> bugStatus x == Pending) bugs
  
-- | Fetches all the bugs from the data store that were submitted by the
-- provided userId
getBugsByUser :: Int64 -- ^ the userId to fetch submitted bugs for
              -> IO [ApiTypes.Bug]
getBugsByUser = SQLiteProvider.selectBugsByUser

-- | Fetches all builds from the data store
getBuilds :: IO [ApiTypes.BuildT]
getBuilds = do
    map (\(x,y,z) -> ApiTypes.BuildT x y z) <$> SQLiteProvider.selectBuilds

-- | Fetches all spells from the data store
getSpells :: IO [ApiTypes.SpellT]
getSpells = SQLiteProvider.selectSpells

-- | Attempts to insert a User in to the data store
storeUser :: AuthTypes.User -- ^ the user to insert
           -> IO (Either IOHError Int64)
storeUser user = do
    uId <- SQLiteProvider.insertUser user
    case uId of
        Just id -> pure $ Right id
        Nothing -> pure $ Left (IOHInsertError "Failed")

-- | Attempts to fetch a user from the data store
getUser :: Int64 -- ^ unique UserId of the user to fetch
        -> IO (Either IOHError AuthTypes.User)
getUser uId = do
    res <- SQLiteProvider.selectUser uId
    case res of
        Just user -> pure $ Right user
        Nothing   -> pure $ Left (IOHFetchError "Failed")

-- | Attempts to fetch a user from the data store (by name)
getUserByName :: String -- ^ unique username of the user to fetch
              -> IO (Either IOHError AuthTypes.User)
getUserByName name = do
    res <- SQLiteProvider.selectUserByName name
    case res of
        Just user -> pure $ Right user
        Nothing   -> pure $ Left (IOHFetchError "Failed")

-- | Fetches all users fromthe data store
getUsers :: IO [AuthTypes.UserL]
getUsers = SQLiteProvider.selectUsers

-- | Fetches a subset of User properties from the data store (for User listing)
getUserL :: Int64 -- ^ unique userId of the user to fetch
         -> IO (Either IOHError AuthTypes.UserL)
getUserL uId = do
    res <- SQLiteProvider.selectUserL uId
    case res of
        Just user -> pure $ Right user
        Nothing   -> pure $ Left (IOHFetchError ("Failed to fetch user with ID " ++ show uId))

-- | (Mod only) Attempts to change the userRole of a given user
changeUserRole :: AuthenticatedUser -- ^ The requesting user (Mod Only)
               -> Int64 -- ^ the unique userId of the user to change role for
               -> Int -- ^ the userRole to change to 
               -> IO (Either IOHError Int64)
changeUserRole au userId role = do
    if not (AuthTypes.isUserMod au) then
        pure $ Left (IOHAuthError "Insufficient permissions")
    else do
        user <- getUser userId
        case user of
            Left (IOHFetchError msg) -> pure $ Left (IOHUpdateError msg)
            Right u -> do
                if userRole u == AuthTypes.userAdminRole && not (AuthTypes.isUserAdmin au) then
                    pure $ Left (IOHAuthError "Mods cannot demote/promote admins")
                else do
                    res <- catch (SQLiteProvider.updateUserRole userId role) (\e -> do
                        putStrLn $ "IOError on changeUserRole " ++ show (e :: IOException)
                        pure False)
                    if res then pure $ Right userId else pure $ Left (IOHUpdateError ("Failed to update user \
                                    \ " ++ show userId ++ " with role " ++ show role))

{-- Comments --}

-- | Attempts to insert a Comment into the data store
storeComment :: AuthenticatedUser -- ^ the requesting user
             -> ApiTypes.Comment -- ^ the comment to insert
             -> IO (Either IOHError Int64)
storeComment au cm = do
    if not (AuthTypes.isUserInit au) then
        pure $ Left (IOHAuthError "Insufficient permissions")
    else if 1000 < length (cBody cm) then
        pure $ Left (IOHInsertError "Comment bodies can only be 500 characters long")
    else do
        putStrLn "[IOH] Attempting to store Comment.."
        cId <- SQLiteProvider.insertComment cm
        case cId of
            Just id -> pure $ Right id
            Nothing -> pure $ Left (IOHInsertError "Failed to insert comment")

-- | Fetches all comments for a given bugId
getComments :: Int64 -- ^ the unique bugId to fetch comments for
            -> IO (Either IOHError [ApiTypes.Comment])
getComments bugId = do
    res <- catch (SQLiteProvider.selectComments bugId) (\e -> do
        putStrLn $ "IOError on getComments " ++ show (e :: IOException)
        pure [])
    pure $ Right res

-- | Deletes a single comment from the data store
deleteComment :: AuthenticatedUser -- ^ the requesting user
              -> Int64 -- ^ The id of the comment to delete
              -> IO (Either IOHError Bool)
deleteComment au cId = do
    if not (AuthTypes.isUserInit au) then
        pure $ Left (IOHAuthError "Insufficient permissions")
    else do
        c <- SQLiteProvider.selectComment cId
        case c of
            Nothing -> pure $ Left (IOHFetchError "Comment not found")
            Just x -> do
                if cUserId x /= auId au && not (isUserMod au) then
                    pure $ Left (IOHAuthError "Not allowed")
                else do
                    res <- catch (SQLiteProvider.deleteComment cId) (\e -> do
                        putStrLn $ "IO Error on delete comment " ++ show (e :: IOException)
                        pure False)
                    pure $ Right True

-- | Fetches the 10 latest comments
getLatestComments :: IO [ApiTypes.Comment]
getLatestComments = SQLiteProvider.selectLatestComments

-- | Gets statistics for number of bugs
getBugStats :: Bool -- ^ flag to include closed and archived bugs
            -> IO (Either IOHError ApiTypes.BugStats)
getBugStats all = do
  res <- SQLiteProvider.selectBugStats all
  pure $ Right res

