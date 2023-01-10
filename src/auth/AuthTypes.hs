{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

-- | Types used specifically for Authentication and User management
module AuthTypes
    (
      User (..),
      UserL (..),
      UserP (..),
      UserRegistration (..),
      UserLoginData (..),
      AuthResultT (..),
      UserChangeRoleData (..),
      AuthenticatedUser (..),
      isUserMod,
      isUserAdmin,
      userModRole,
      userAdminRole,
      isUserInit,
      isUserNovice
    ) where

---
import Data.Aeson ( FromJSON, ToJSON, defaultOptions )
import Data.Aeson.TH ( defaultOptions, deriveJSON )
import Data.Int ( Int64 )
import Data.UUID ( UUID )
import Servant.Auth.Server ( FromJWT, ToJWT )
import GHC.Generics ( Generic )
---
import ApiTypes ( Bug )
---

-- | Represents a single User
data User = User
  { userId :: Int64,
    userCreatedTime :: Maybe String,
    userLastLogin :: Maybe String,
    userUuid :: UUID,
    userHash :: String,
    userSalt :: String,
    userName :: String,
    userRole :: Int,
    userAvatar :: Int
  } deriving (Show)

-- | Subset of User properties used in User Listings
data UserL = UserL
  { userLId :: Int64,
    userLName :: String,
    userLRole :: Int,
    userLCreatedTime :: String,
    userLLastLogin :: Maybe String,
    userLAvatar :: Int
  } deriving (Show)
$(deriveJSON defaultOptions ''UserL)

-- | Subset of User properties used in User Profiles
data UserP = UserP
  { userPId :: Int64,
    userPName :: String,
    userPRole :: Int,
    userPCreatedTime :: String,
    userPLastLogin :: Maybe String,
    userPAvatar :: Int,
    userPBugs :: [ApiTypes.Bug]
  } deriving (Show)
$(deriveJSON defaultOptions ''UserP)

-- | Submitted when attempting to create a new User
data UserRegistration = UserRegistration
    { userRegName :: String,
      userRegEmail :: String,
      userRegPass :: String,
      userRegAvatar :: Int
    } deriving (Show)
$(deriveJSON defaultOptions ''UserRegistration)

-- | Submitted when attempting to authenticate
data UserLoginData = UserLoginData
    { uldName :: String,
      uldPass :: String
    } deriving (Show)
$(deriveJSON defaultOptions ''UserLoginData)

-- | Payload for changing a user role (mod only)
data UserChangeRoleData = UserChangeRoleData
    { ucrId :: Int64,
      ucrRole :: Int
    } deriving (Show)
$(deriveJSON defaultOptions ''UserChangeRoleData)

-- | Primary auth type, encoded in Bearer tokens in API calls
data AuthenticatedUser = AUser
    { auId :: Int64,
      auRole :: Int,
      auName :: String,
      auSecret :: String,
      auLoginTime :: String
    } deriving (Show, Generic)
instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

-- | Returned from the AuthHandler's tryAuthenticate function
data AuthResultT
    = AuthOK String AuthenticatedUser
    | AuthFailed String
    | AuthError String
    deriving (Show)

-- | The minimum role of a moderator
userModRole :: Int
userModRole = 3

-- | The minimum role of an admin
userAdminRole :: Int
userAdminRole = 5

-- | Checks if a user is at least novice
isUserNovice :: AuthenticatedUser -> Bool
isUserNovice au = auRole au >= 1

-- | Checks if a user is at least initiated
isUserInit :: AuthenticatedUser -> Bool
isUserInit au = auRole au >= 2

-- | Checks if a user is at least moderator
isUserMod :: AuthenticatedUser -> Bool
isUserMod au = auRole au >= userModRole

-- | Checks if a user is at least administrator
isUserAdmin :: AuthenticatedUser -> Bool
isUserAdmin au = auRole au >= userAdminRole
