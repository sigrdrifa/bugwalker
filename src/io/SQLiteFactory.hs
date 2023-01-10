{-# LANGUAGE OverloadedStrings #-}

-- | Contains functions for creating the underlying SQLite datastore.
module SQLiteFactory
    (
      createDb,
      dbPath
    ) where
---
import Database.SQLite.Simple
    ( FromRow(..),
      ToRow(..),
      SQLData(SQLInteger, SQLText),
      close,
      execute_,
      open,
      field,
      ResultError(ConversionFailed) )
import Database.SQLite.Simple.FromRow ( FromRow(..), field )
import Database.SQLite.Simple.FromField
    ( FromField(..), ResultError(ConversionFailed), returnError )
import Database.SQLite.Simple.Ok ( Ok(Ok) )
import Database.SQLite.Simple.Internal ( Field(Field) )
import Control.Monad (when)
import Data.UUID ( UUID, fromString, nil )
import Data.Text (unpack)
---
import AuthTypes (
  User (..),
  UserL (..)
  )
import ApiTypes (
  Bug (..),
  BugSeverity (..),
  BugType (..),
  BugStatus (..),
  Specialisation (..),
  BugDetail (..),
  SpellT (..),
  Comment (..))
---

{- Db Type instances -}

instance FromField BugSeverity where
  fromField (Field (SQLInteger x) _) = Ok (toEnum (fromIntegral x) :: BugSeverity)
  fromField f                        = returnError ConversionFailed f "BugSeverity needs an Int"

instance FromField BugType where
  fromField (Field (SQLInteger x) _) = Ok (toEnum (fromIntegral x) :: BugType)
  fromField f                        = returnError ConversionFailed f "BugType needs an Int"

instance FromField BugStatus where
  fromField (Field (SQLInteger x) _) = Ok (toEnum (fromIntegral x) :: BugStatus)
  fromField f                        = returnError ConversionFailed f "BugStatus needs an Int"

instance FromField Specialisation where
  fromField (Field (SQLInteger x) _) = Ok (toEnum (fromIntegral x) :: Specialisation)
  fromField f                        = returnError ConversionFailed f "Specialisation needs an Int"

instance FromRow Int where
  fromRow = field

instance FromRow Bug where
  fromRow = Bug <$> field <*> field <*> field <*> field <*> field <*> field
            <*> field <*> field <*> field <*> field <*> field <*> field
            <*> field <*> field <*> field

instance FromRow BugDetail where
  fromRow = BugDetail <$> field <*> field <*> field <*> field <*> field <*> field
                      <*> field <*> field <*> field <*> field <*> field <*> field
                      <*> field <*> field <*> field <*> field

instance FromRow SpellT where
  fromRow = SpellT <$> field <*> field <*> field <*> field

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field <*> field
                    <*> field <*> field <*> field <*> field <*> field

instance FromField UUID where
  fromField (Field (SQLText x) _) =
      case Data.UUID.fromString $ unpack x of
          Just uuid -> Ok uuid
          Nothing   -> Ok nil
  fromField f = returnError ConversionFailed f "Failed to Convert text to UUID"

instance FromRow AuthTypes.User where
  fromRow = AuthTypes.User <$> field <*> field <*> field <*> field <*> field
                           <*> field <*> field <*> field <*> field

instance ToRow AuthTypes.User where
  toRow (AuthTypes.User _uId uCt uLl uUuid uH uS uN uR uA) =
    toRow (uLl, show uUuid, uH, uS, uN, uR, uA)

instance FromRow AuthTypes.UserL where
  fromRow = AuthTypes.UserL <$> field <*> field <*> field <*> field <*> field <*> field

-- | The default path of the sqlite data store
dbPath :: String
dbPath = "data/bw.db"

{- Schema -}

-- | Creates a new db or connects to an existing one at the default path
-- and creates the basic table structure 
createDb :: Bool -- ^ Whether or not asset tables should be dropped
         -> IO ()
createDb isHardInit = do
  putStrLn $ "Connecting to db " ++ dbPath ++ "..."
  conn <- open dbPath
  when isHardInit $ do
    putStrLn "Dropping spells and builds tables."
    execute_ conn "DROP TABLE IF EXISTS spells;"
    execute_ conn "DROP TABLE IF EXISTS builds;"

  putStrLn "Done. Creating tables..."
  execute_ conn "CREATE TABLE IF NOT EXISTS spells ( \
       \id INTEGER PRIMARY KEY,\
       \buildId INTEGER,\
       \name TEXT,\
       \description TEXT\
       \);"
  execute_ conn "CREATE TABLE IF NOT EXISTS builds ( \
       \id INTEGER PRIMARY KEY AUTOINCREMENT,\
       \buildString TEXT UNIQUE,\
       \date TEXT,\
       \record_time NOT NULL DEFAULT CURRENT_TIMESTAMP\
       \);"
  execute_ conn "CREATE TABLE IF NOT EXISTS bugs ( \
      \id INTEGER PRIMARY KEY AUTOINCREMENT,\
      \created_time NOT NULL DEFAULT CURRENT_TIMESTAMP,\
      \time_modified NOT NULL DEFAULT CURRENT_TIMESTAMP,\
      \severity INTEGER,\
      \type INTEGER,\
      \title TEXT UNIQUE,\
      \status INTEGER DEFAULT 4,\
      \tags TEXT,\
      \spellId INTEGER,\
      \buildId INTEGER,\
      \spec INTEGER,\
      \description TEXT,\
      \steps TEXT,\
      \content TEXT,\
      \blueTrackerLink TEXT,\
      \submittedBy INTEGER NOT NULL,\
      \FOREIGN KEY(spellId) REFERENCES spells(id),\
      \FOREIGN KEY(buildId) REFERENCES builds(id)\
      \);"
  execute_ conn "CREATE TABLE IF NOT EXISTS users ( \
      \id INTEGER PRIMARY KEY AUTOINCREMENT,\
      \created_time DEFAULT CURRENT_TIMESTAMP,\
      \last_login DEFAULT CURRENT_TIMESTAMP,\
      \uuid TEXT UNIQUE NOT NULL,\
      \hash TEXT,\
      \salt TEXT,\
      \username TEXT UNIQUE,\
      \role INTEGER DEFAULT 1,\
      \avatar INTEGER DEFAULT 1\
      \);"
  execute_ conn "CREATE TABLE IF NOT EXISTS comments ( \
      \id INTEGER PRIMARY KEY AUTOINCREMENT,\
      \created_time DEFAULT CURRENT_TIMESTAMP,\
      \last_modified DEFAULT CURRENT_TIMESTAMP,\
      \userId INTEGER NOT NULL,\
      \bugId INTEGER NOT NULL,\
      \status INTEGER NOT NULL,\
      \body TEXT,\
      \FOREIGN KEY(userId) REFERENCES users(id),\
      \FOREIGN KEY(bugId) REFERENCES bugs(id)\
      \);"
  putStrLn "Done. Closing."
  close conn

