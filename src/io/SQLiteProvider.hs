{-# LANGUAGE OverloadedStrings #-}

-- |  Contains functions for interacting with the underlying SQLite datastore.
module SQLiteProvider
    ( createDb,
      insertSpellData,
      selectBuilds,
      selectSpells,
      insertBug,
      selectBug,
      updateBug,
      deleteBug,
      selectBugs,
      selectBugs',
      insertUser,
      selectUser,
      selectUserL,
      selectUserByName,
      selectBugsByUser,
      selectUsers,
      updateUserRole,
      insertComment,
      selectComment,
      selectComments,
      deleteComment,
      deleteCommentsByUser,
      selectBugStats,
      selectLatestComments
    ) where
---
import Database.SQLite.Simple
    ( close,
      execute,
      executeNamed,
      lastInsertRowId,
      open,
      query,
      query_,
      withConnection,
      Only(Only),
      NamedParam((:=)),
      Connection )
import Database.SQLite.Simple.Internal ( Connection )
import Data.Int ( Int64 )
---
import SQLiteFactory (createDb, dbPath)
import GameDataTypes
    ( toBuildString,
      Build(date),
      Spell(spellId, spellName, spellDesc),
      SpellData(build, spells) )
import AuthTypes (
  User (..),
  UserL (..))
import ApiTypes (
  Bug (..),
  BugSeverity (..),
  BugType (..),
  BugStatus (..),
  Specialisation (..),
  BugDetail (..),
  SpellT (..),
  Comment (..),
  BugStats (..))
---

{-- Bugs --}

-- | Inserts a single bug into the database
-- @todo refactor to use ToRow
insertBug :: Int -- ^ the severity of the bug
          -> Int -- ^ the type of the bug
          -> Int -- ^ the status of the bug
          -> String -- ^ the title of the bug
          -> String -- ^ the tags of the bug
          -> Int -- ^ the spellId of the bug
          -> Int -- ^ the buildId of the bug
          -> Int -- ^ the Specilalisation of the bug
          -> String -- ^ the description of the bug
          -> String -- ^ the steps of the bug
          -> String -- ^ the content of the bug
          -> String -- ^ the blueTrackerLink of the bug
          -> Int64 -- ^ the userId that created the bug
          -> IO (Maybe Int64) -- ^ On success: The id that the bug was assigned in the db
insertBug severity t status title tags spellId buildId spec desc steps content btl user = do
  withConnection dbPath $ \conn -> do
    execute conn "INSERT INTO bugs (severity,type,status,title,\
    \tags,spellId,buildId,spec,description,steps,content,blueTrackerLink,submittedBy)\
    \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)\
    \" [show severity, show t, show status, title, tags, show spellId, show buildId,
        show spec, desc, steps, content, btl, show user]
    id <- lastInsertRowId conn
    if id > -1 then return $ Just id else return Nothing

updateBug :: Int64 -> BugDetail -> IO (Maybe Int64)
updateBug bId bd = do
  let t = bugdtTitle bd
  let sev = fromEnum $ bugdtSeverity bd
  let bt = fromEnum $ bugdtType bd
  let desc = bugdtDescription bd
  let ct = bugdtContent bd
  let ta = bugdtTags bd
  let status = fromEnum $ bugdtStatus bd
  withConnection dbPath $ \conn -> do
    executeNamed conn "UPDATE bugs SET title = :t, description = :desc, \ 
    \content = :ct, status = :st, time_modified = datetime('now'), \
    \type = :bt, severity = :sev, tags = :ta \
    \WHERE id = :id" [":t" := t, ":desc" := desc, ":ct" := ct, ":st" := status, "\
    \:id" := bId, ":bt" := bt, ":sev" := sev, ":ta" := ta]
  return $ Just bId

selectBug :: Int64 -> IO (Maybe ApiTypes.BugDetail)
selectBug bId = do
  withConnection dbPath $ \conn -> do
    r <- query conn "SELECT * FROM bugs WHERE id = ?" (Only bId) :: IO [ApiTypes.BugDetail]
    case r of
      [x] -> return (Just x)
      [] -> return Nothing

deleteBug :: Int64 -> IO Bool
deleteBug bId = do
  withConnection dbPath $ \conn -> do
    execute conn "DELETE FROM bugs WHERE id = ?" (Only bId)
    return True

selectBugs :: Int -> IO [ApiTypes.Bug]
selectBugs limit = do
  withConnection dbPath $ \conn -> do
    query conn "SELECT id,created_time,severity,type,title,status,tags,spellId,\
    \buildId,spec,blueTrackerLink FROM bugs ORDER BY created_time DESC \
    \LIMIT ?" (Only limit) :: IO [ApiTypes.Bug]

selectBugs' :: Int -> IO [ApiTypes.Bug]
selectBugs' limit = do
  withConnection dbPath $ \conn -> do
    query conn "SELECT bugs.id,bugs.created_time,severity,type,title,status,tags,spellId,\
    \bugs.buildId,spec,blueTrackerLink,name,username,submittedBy,buildString\
    \ FROM bugs INNER JOIN spells ON bugs.spellId = spells.Id INNER JOIN users ON \
    \bugs.submittedBy = users.id INNER JOIN builds on bugs.buildId = builds.id \
    \ORDER BY bugs.created_time DESC LIMIT ?" (Only limit) :: IO [ApiTypes.Bug]

selectBugsByUser :: Int64 -> IO [ApiTypes.Bug]
selectBugsByUser uId = do
  withConnection dbPath $ \conn -> do
    query conn "SELECT bugs.id,bugs.time_modified,severity,type,title,status,tags,spellId,\
    \bugs.buildId,spec,blueTrackerLink,name,username,submittedBy,buildString\
    \ FROM bugs INNER JOIN spells ON bugs.spellId = spells.Id INNER JOIN users ON \
    \bugs.submittedBy = users.id INNER JOIN builds on bugs.buildId = builds.id \
    \WHERE submittedBy = ?" (Only uId) :: IO [ApiTypes.Bug]

{-- Builds --}

insertBuild :: Connection -> Build -> IO (Maybe Int64)
insertBuild conn build = do
  let buildStr = GameDataTypes.toBuildString build
  let date = GameDataTypes.date build
  execute conn "INSERT OR REPLACE INTO builds (buildString, date) VALUES (?,?)" [buildStr, date]
  id <- lastInsertRowId conn
  if id > -1 then return $ Just id else return Nothing

selectBuilds :: IO [(Int,String,String)]
selectBuilds = do
  withConnection dbPath $ \conn -> do
    query_ conn "SELECT id,buildString,date FROM builds" :: IO [(Int,String,String)]

{-- Spells --}

selectSpells :: IO [SpellT]
selectSpells = do
  withConnection dbPath $ \conn -> do
    query_ conn "SELECT spells.id,name,description,buildString \
    \from spells INNER JOIN builds ON builds.id = buildId WHERE \
    \description NOT LIKE '$@spelldesc%'" :: IO [SpellT]

insertSpell :: Connection -> Int64 -> Spell -> IO ()
insertSpell conn bId sp = do
    let strId = show $ spellId sp
    let spName = spellName sp
    let spDesc = spellDesc sp
    let spBuildId = show bId
    execute conn "INSERT OR REPLACE INTO spells\
    \ (id, buildId, name, description) VALUES (?,?,?,?)" [strId, spBuildId, spName, spDesc]

insertSpellData :: SpellData -> IO ()
insertSpellData spd = do
    putStrLn $ "Connecting to db " ++ dbPath ++ " to insert SpellData.."
    putStrLn $ "SpellData has " ++ show (length (spells spd)) ++ " spells to insert."
    conn <- open dbPath
    buildId <- insertBuild conn $ GameDataTypes.build spd
    case buildId of
      Just bId -> do mapM_ (insertSpell conn bId) $ spells spd
      Nothing -> putStrLn "Failed to retrieve Build ID. No spelldata inserted."
    close conn

{-- Users --}

insertUser :: AuthTypes.User -> IO (Maybe Int64)
insertUser user = do
  withConnection dbPath $ \conn -> do
    execute conn "INSERT INTO users (created_time,last_login,uuid,\
      \hash,salt,username,role,avatar) VALUES (datetime('now'),?,?,?,?,?,?,?)" user
    userId <- lastInsertRowId conn
    if userId > -1 then pure (Just userId) else return Nothing

selectUser :: Int64 -> IO (Maybe AuthTypes.User)
selectUser uId = do
  withConnection dbPath $ \conn -> do
    r <- query conn "SELECT * FROM users WHERE id = ?" (Only uId) :: IO [AuthTypes.User]
    case r of
      [x] -> return (Just x)
      _ -> return Nothing

selectUserL :: Int64 -> IO (Maybe AuthTypes.UserL)
selectUserL uId = do
    withConnection dbPath $ \conn -> do
      r <- query conn "SELECT id,username,role,created_time,last_login,avatar FROM users WHERE id = ?" (Only uId) :: IO [AuthTypes.UserL]
      case r of
        [x] -> return (Just x)
        _ -> return Nothing

selectUserByName :: String -> IO (Maybe AuthTypes.User)
selectUserByName name = do
  withConnection dbPath $ \conn -> do
    r <- query conn "SELECT * FROM users WHERE username = ?" (Only name) :: IO [AuthTypes.User]
    case r of
      [x] -> return (Just x)
      _ -> return Nothing

selectUsers :: IO [AuthTypes.UserL]
selectUsers = do
  withConnection dbPath $ \conn -> do
    query_ conn "SELECT id,username,role,created_time,last_login,avatar\
                     \ FROM users LIMIT 100000" :: IO [AuthTypes.UserL]

updateUserRole :: Int64 -> Int -> IO Bool
updateUserRole userId role = do
  withConnection dbPath $ \conn -> do
    executeNamed conn "UPDATE users SET role = :role WHERE id = :id\
    \ " [":role" := role, ":id" := userId]
    pure True

{-- Comments --}

insertComment :: ApiTypes.Comment -> IO (Maybe Int64)
insertComment cm = do
  withConnection dbPath $ \conn -> do
    execute conn "INSERT INTO comments (created_time, last_modified, userId,\
    \bugId,status,body) VALUES (datetime('now'),datetime('now'),?,?,1,?)\
    \ " [show $ cUserId cm, show $ cBugId cm, cBody cm]
    commentId <- lastInsertRowId conn
    if commentId > -1 then pure (Just commentId) else return Nothing

-- | Attempts to fetch a single comment from the data store
selectComment :: Int64 -- ^ the id of the comment to fetch
              -> IO (Maybe ApiTypes.Comment)
selectComment cId = do
  withConnection dbPath $ \conn -> do
    r <- query conn "SELECT comments.id,comments.created_time,comments.last_modified\
    \,comments.userId,username,role,avatar,comments.bugId,comments.status,body\
    \ FROM comments INNER JOIN users ON comments.userId = users.id \
    \WHERE comments.id = ?" (Only cId) :: IO [ApiTypes.Comment]
    case r of
      [x] -> pure $ Just x
      _ -> pure Nothing

selectComments :: Int64 -> IO [ApiTypes.Comment]
selectComments bugId = do
  withConnection dbPath $ \conn -> do
    query conn "SELECT comments.id,comments.created_time,comments.last_modified\
    \,comments.userId,username,role,avatar,comments.bugId,comments.status,body\
    \ FROM comments INNER JOIN users ON comments.userId = users.id \
    \WHERE comments.bugId = ?" (Only bugId) :: IO [ApiTypes.Comment]

-- | Deletes a single comment from the data store
deleteComment :: Int64 -- ^ the id of the comment to delete
              -> IO Bool
deleteComment cId = do
  withConnection dbPath $ \conn -> do
    execute conn "DELETE FROM comments WHERE id = ?" (Only cId)
    return True

-- | Deletes all comments submitted by a User (used when deleting the user)
deleteCommentsByUser :: Int64 -- ^ the id of the user to delete comments for
                     -> IO Bool
deleteCommentsByUser uId = do
    withConnection dbPath $ \conn -> do
        execute conn "DELETE FROM comments WHERE userId = ?" (Only uId)
        pure True

-- | Get latest comments
selectLatestComments :: IO [ApiTypes.Comment]
selectLatestComments = do
  withConnection dbPath $ \conn -> do
    query_ conn "SELECT comments.id,comments.created_time,comments.last_modified\
      \,comments.userId,username,role,avatar,comments.bugId,comments.status,body\
      \ FROM comments INNER JOIN users ON comments.userId = users.id ORDER BY comments.id DESC LIMIT 10"

-- | Gets statistics for all the stored bugs
selectBugStats :: Bool -- ^ flag to include closed and archived bugs 
               -> IO ApiTypes.BugStats
selectBugStats all = do
  withConnection dbPath $ \conn -> do
    [allBugs] <- query_ conn "SELECT COUNT(*) FROM bugs WHERE spec = 0 AND status = 1" :: IO [Int]
    [wwBugs] <- query_ conn "SELECT COUNT(*) FROM bugs WHERE spec = 1 AND status = 1" :: IO [Int]
    [mwBugs] <- query_ conn "SELECT COUNT(*) FROM bugs WHERE spec = 3 AND status = 1" :: IO [Int] 
    [bmBugs] <- query_ conn "SELECT COUNT(*) FROM bugs WHERE spec = 2 AND status = 1" :: IO [Int]
    [lowBugs] <- query_ conn "SELECT COUNT(*) FROM bugs WHERE severity = 0 AND status = 1" :: IO [Int] 
    [medBugs] <- query_ conn "SELECT COUNT(*) FROM bugs WHERE severity = 1 AND status = 1" :: IO [Int] 
    [critBugs] <- query_ conn "SELECT COUNT(*) FROM bugs WHERE severity = 2 AND status = 1" :: IO [Int] 
    return $ ApiTypes.BugStats wwBugs mwBugs bmBugs allBugs (allBugs + wwBugs + mwBugs + bmBugs) lowBugs medBugs critBugs 
