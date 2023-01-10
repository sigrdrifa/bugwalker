-- | module handling authentication and creation of users
module AuthHandler
    (
      createUser,
      tryAuthenticate
    ) where
---
import Data.UUID.V4 ( nextRandom )
import Data.UUID ( UUID )
import qualified Data.ByteString as B hiding (foldr, map)
import Data.ByteString.Char8 (pack)
import Data.Char ( isAlpha, toLower )
import Crypto.Hash ( hashWith, SHA256(SHA256) )
---
import AuthTypes
    ( AuthResultT(AuthFailed, AuthOK),
      AuthenticatedUser(AUser),
      User(User, userHash, userUuid, userId, userRole, userName),
      UserRegistration(userRegName, userRegPass, userRegAvatar) )
import Utils ( ApiError(ValidationError), ValidationResult(..) )
---

class Monad m => UserPrinter m where
  printUser :: User -> m ()

instance UserPrinter IO where
  printUser s = print s

class Monad m => UUIDGenerator m where
  newUUID :: m UUID

instance UUIDGenerator IO where
  newUUID = nextRandom

-- | Validates username input
validateUserName :: String -- ^ the username input to validate
                 -> ValidationResult
validateUserName n
  | length n < 3 =
    Invalid "Username must be at least 3 characters."
  | not (foldr ((&&) . isAlpha) True n) =
    Invalid "Username can only contain letters."
  | otherwise = Valid

-- | Validates password input
validatePassword :: B.ByteString -- ^ the password to validate
                 -> ValidationResult
validatePassword p =
    if B.length p < 8 then
        Invalid "Passwords should be longer than 8 characters."
    else Valid

-- | generates a SH256 hash of the password using the unique user
-- UUID as a salt
generateHashStr :: String -- ^ string representation of password
                -> String -- ^ string repreentation of user UUID
                -> String -- ^ the resulting SHA256 hash
generateHashStr p u = show $ hashWith SHA256 $ pack $ p ++ u

-- | Attempts to validate and create a new User object from the given registration data
createUser :: (UserPrinter m, UUIDGenerator m) => UserRegistration -- ^ UserRegistration to create
           -> Int -- ^ Role of the user to create 
           -> m (Either ApiError User)
createUser ur uRole =
    case (validateUserName uName,
          validatePassword uPass)
    of
        (Invalid r1, Invalid r2) -> pure $ Left $ ValidationError "username password" (r1 ++ " | " ++ r2)
        (Valid, Invalid r2)      -> pure $ Left $ ValidationError "password" r2
        (Invalid r1, Valid)      -> pure $ Left $ ValidationError "username" r1
        (Valid, Valid)           -> do
            uuid <- newUUID
            let hash = generateHashStr (show uPass) (show uuid)
                user = User 0 Nothing Nothing uuid hash "" (map toLower uName) uRole uAvatar
            printUser user
            pure $ Right user
    where
        uName = userRegName ur
        uPass = pack $ userRegPass ur
        uAvatar = userRegAvatar ur

-- | Attempts to authenticate a given user with the given password
tryAuthenticate :: User -- ^ The user to authenticate
                -> String -- ^ string representation of the password
                -> String -- ^ the current dt to use if login succeeds
                -> AuthResultT -- ^ the authentication result
tryAuthenticate user p dt =
    if uHash == generateHashStr (show $ pack p) (show uuid) then AuthOK "" (AUser uId uRole uName "If Henry went to \
                          \Hogwarts, he would be in Slytherin" dt) else AuthFailed ("Invalid password for User " ++ userName user ++ ".")
    where
        uHash = userHash user
        uuid = userUuid user
        uId = userId user
        uRole = userRole user
        uName = userName user

