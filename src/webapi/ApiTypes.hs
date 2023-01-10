{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

-- | Types used in the BugWalker API
module ApiTypes
    ( 
      BuildT (..),
      SpellT (..),
      Bug (..),
      BugDetail (..),
      BugSeverity (..),
      BugType (..),
      Specialisation (..),
      BugStatus (..),
      ResponseMessage (..),
      LoginResponseMessage (..),
      BugChangedResponseMessage (..),
      UserChangedResponseMessage (..),
      Comment (..),
      BugStats (..)
    ) where
--
import Data.Aeson ( defaultOptions )
import Data.Aeson.TH ( defaultOptions, deriveJSON )
import Data.Int ( Int64 )
--

{- Data Types -}

-- | Represents the possible Specilisations that a monk can have
data Specialisation = All | Windwalker | Brewmaster | Mistweaver deriving (Enum, Show)
$(deriveJSON defaultOptions ''Specialisation)

-- | Represents the statuses that a Bug can have
data BugStatus = Closed | Open | Verified | Archived | Pending deriving (Enum, Show, Eq)
$(deriveJSON defaultOptions ''BugStatus)

-- | Represents the severity levels that a Bug can have
data BugSeverity = Low | Medium | Critical deriving (Enum, Show)
$(deriveJSON defaultOptions ''BugSeverity)

-- | Represents the different types that a Bug can have
data BugType = Visual | Mechanical | Gameplay | System deriving (Enum, Show)
$(deriveJSON defaultOptions ''BugType)

-- | Primary type of a single Bug
data Bug = Bug
  { bugId :: Int64,
    bugDateModified :: String,
    bugSeverity :: BugSeverity,
    bugType :: BugType,
    bugTitle :: String,
    bugStatus :: BugStatus,
    bugTags :: Maybe String,
    bugSpellId :: Int,
    bugBuildId :: Int,
    bugSpec :: Specialisation,
    bugBlueTrackerLink :: Maybe String,
    bugSpellName :: Maybe String,
    bugUserName :: Maybe String,
    bugUserId :: Int64,
    bugBuildString :: Maybe String
  } deriving (Show)
$(deriveJSON defaultOptions ''Bug)

-- | Detailed representation of a Bug including the Content fields
data BugDetail = BugDetail
  { bugdtId :: Int64,
    bugdtDateCreated :: String,
    bugdtDateModified :: Maybe String,
    bugdtSeverity :: BugSeverity,
    bugdtType :: BugType,
    bugdtTitle :: String,
    bugdtStatus :: BugStatus,
    bugdtTags :: Maybe String,
    bugdtSpellId :: Int,
    bugdtBuildId :: Int,
    bugdtSpec :: Specialisation,
    bugdtDescription :: Maybe String,
    bugdtSteps :: Maybe String,
    bugdtContent :: Maybe String,
    bugdtBlueTrackerLink :: Maybe String,
    bugdtSubmitter :: Int64
  } deriving (Show)
$(deriveJSON defaultOptions ''BugDetail)

-- | Api representation of a GameData build
data BuildT = BuildT
  { buildIdT       :: Int,
    buildStringT   :: String,
    buildDateT     :: String
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''BuildT)

-- | Api representation of a GameData Spell
data SpellT = SpellT {
    spellIdT :: Int, 
    spellNameT :: String,
    spellDescT :: String,
    spellBuildT :: String
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''SpellT)

-- | Api general ResponseMessage
data ResponseMessage = ResponseMessage {
    resMessageStatusCode :: Int,
    resMessageStatus :: String,
    resMessage :: String
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''ResponseMessage)

-- | Api response type to a login request
data LoginResponseMessage = LoginResponseMessage {
    lrmStatusCode :: Int,
    lrmMessage :: String,
    lrmToken :: String
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''LoginResponseMessage)

-- | Api response type to a Bug insertion, delete or update
data BugChangedResponseMessage = BugChangedResponseMessage {
    bcMessageStatusCode :: Int,
    bcMessageBugId :: Int64,
    bcMessageStatus :: String,
    bcMessage :: String
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''BugChangedResponseMessage)

-- | Api response type to a User insertion, delete or update
data UserChangedResponseMessage = UserChangedResponseMessage {
    ucMessageStatusCode :: Int,
    ucMessageUserId :: Int64,
    ucMessageStatus :: String,
    ucMessage :: String
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''UserChangedResponseMessage)

-- | Representation of a single Comment
data Comment = Comment {
  cId :: Int64,
  cCreatedTime :: String,
  cModifiedTime :: String,
  cUserId :: Int64,
  cUserName :: String,
  cUserRole :: Int,
  cUserAvatar :: Int,
  cBugId :: Int64,
  cStatus :: Int,
  cBody :: String
} deriving (Show)
$(deriveJSON defaultOptions ''Comment)

-- | API response to bug number stats
data BugStats = BugStats {
  bsNumOpenWindwalker :: Int,
  bsNumOpenMistweaver :: Int,
  bsNumOpenBrewmaster :: Int,
  bsNumOpenAll :: Int,
  bsNumOpenTotal :: Int,
  bsNumOpenLow :: Int,
  bsNumOpenMedium :: Int,
  bsNumOpenCritical :: Int
} deriving (Show)
$(deriveJSON defaultOptions ''BugStats)

