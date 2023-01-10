-- | Handlers related to User API endpoints
module UserApi
    (
      userRegistrationHandler,
      userLoginHandler,
      getUsersHandler,
      getUserPHandler,
      changeUserRoleHandler,
      getUserSecretHandler
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
import Servant.Auth.Server ( makeJWT, JWTSettings )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.ByteString (pack)
import Data.ByteString.Lazy.Internal (packChars, unpackChars)
import Data.Int ( Int64 )
import Data.Char ( toLower )
import Data.Time
---
import AuthTypes
    ( isUserMod,
      AuthResultT(AuthFailed, AuthOK),
      AuthenticatedUser(auLoginTime),
      User(userUuid),
      UserChangeRoleData(ucrRole, ucrId),
      UserL(userLName, userLRole, userLCreatedTime, userLLastLogin,
            userLAvatar),
      UserLoginData(uldName, uldPass),
      UserP(UserP),
      UserRegistration )
import AuthHandler ( createUser, tryAuthenticate )
import IOH
    ( changeUserRole,
      getBugsByUser,
      getUserByName,
      getUserL,
      getUsers,
      storeUser,
      IOHError(IOHAuthError, IOHFetchError) )
import ApiTypes
    ( LoginResponseMessage(LoginResponseMessage),
      UserChangedResponseMessage(UserChangedResponseMessage) )
import Utils ( ApiError(ValidationError) )
----

-- | Handles user registration requests by asking the AuthHandler to create
-- the user and then asks the IOH to store the new user 
userRegistrationHandler :: UserRegistration -- ^ UserRegistration object to store
                        -> Handler UserChangedResponseMessage
userRegistrationHandler ur = do
  r <- liftIO $ AuthHandler.createUser ur 1
  case r of
    (Right user) -> do
      dbRes <- liftIO $ IOH.storeUser user
      case dbRes of
        (Right uId) -> pure $ UserChangedResponseMessage 200 uId "OK" $
                       "Created user with UUID " ++ show (userUuid user)
        (Left _)       -> Servant.throwError err500
    (Left (ValidationError field msg)) ->
      Servant.throwError err400 { errBody = packChars (field ++ " | " ++ msg) }

-- | Handles user login by getting the User object from the IOH
-- and asking the AuthHandler to authenticate. Creates a JWT token on success
userLoginHandler :: JWTSettings -- ^ the JWTSettings 
                 -> UserLoginData -- ^ the UserLoginData object
                 -> Handler LoginResponseMessage
userLoginHandler jwts uld = do
    r <- liftIO $ IOH.getUserByName $ map toLower (uldName uld)
    case r of
        (Right user) -> do
            dt <- liftIO getCurrentTime
            let timestamp = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") dt
            case AuthHandler.tryAuthenticate user (uldPass uld) timestamp of
                (AuthOK token uA) -> do
                    etoken <- liftIO $ makeJWT uA jwts Nothing
                    case etoken of
                        Left e -> Servant.throwError err500
                        Right v -> pure $ LoginResponseMessage 200 "OK" (unpackChars v)
                (AuthFailed m) -> Servant.throwError err400 { errBody = packChars m }
        (Left _)     -> Servant.throwError err500

-- | Handles listing users
getUsersHandler :: Handler [AuthTypes.UserL]
getUsersHandler = liftIO IOH.getUsers

-- | Handles feching of UserProfile objects
getUserPHandler :: Int64 -- ^ UserId of the user to fech profile for
                -> Handler AuthTypes.UserP
getUserPHandler uId = do
    u <- liftIO $ IOH.getUserL uId
    case u of
      Right uL -> do
        bugs <- liftIO $ IOH.getBugsByUser uId
        pure $ AuthTypes.UserP uId (userLName uL) (userLRole uL) (userLCreatedTime uL) (userLLastLogin uL) (userLAvatar uL) $ reverse bugs
      Left (IOHFetchError m) -> Servant.throwError err404 { errBody = packChars m }

-- | Validates an auth user and returns the secret 
getUserSecretHandler :: AuthenticatedUser -- ^ | The Auth user to verify
                     -> Handler AuthenticatedUser
getUserSecretHandler au = do
    t <- liftIO getCurrentTime
    let diff = addUTCTime (fromInteger $ -12000) t
    let timestamp = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") diff
    if auLoginTime au < timestamp then
        Servant.throwError err401 { errBody = packChars "Login expired"}
    else
        pure au

-- | Handles changing a user's role (mod only)
changeUserRoleHandler :: AuthenticatedUser -- ^ The user making the request
                      -> UserChangeRoleData -- ^ The user change data to make
                      -> Handler UserChangedResponseMessage
changeUserRoleHandler au ucr =
    if not $ isUserMod au then
      Servant.throwError err404 { errBody = packChars "Insufficient Permissions" }
    else do
      res <- liftIO $ IOH.changeUserRole au (ucrId ucr) (ucrRole ucr)
      case res of
        Right uId -> pure $ UserChangedResponseMessage 200 (ucrId ucr) "OK" $
                    "Changed role for user " ++ show uId
        Left (IOHFetchError msg) -> Servant.throwError err404 { errBody = packChars msg }
        Left (IOHAuthError msg)  -> Servant.throwError err401 { errBody = packChars msg}

