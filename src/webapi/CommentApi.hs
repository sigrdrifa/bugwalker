{-# LANGUAGE OverloadedStrings #-}

-- | Handlers related to Comment API endpoints
module CommentApi
    (
      getCommentsHandler,
      getCommentsByUserHandler,
      postCommentHandler,
      deleteCommentHandler,
      getLatestCommentsHandler
    ) where
---
import Servant
    ( throwError,
      Handler,
      err400,
      err401,
      err500,
      ServerError(errBody) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.ByteString (pack)
import Control.Monad (unless)
import Data.ByteString.Lazy.Internal (packChars)
import Data.Int ( Int64 )
---
import AuthTypes ( isUserInit, AuthenticatedUser )
import IOH
    ( getComments,
      storeComment,
      deleteComment,
      getLatestComments,
      IOHError(IOHInsertError, IOHFetchError) )
import ApiTypes ( Comment, ResponseMessage(ResponseMessage) )
----

-- | Handler for fetching comments for a bugId
getCommentsHandler :: Int64 -- ^ the bugId to fetch comments for
                   -> Handler [ApiTypes.Comment]
getCommentsHandler bugId = do
    r <- liftIO $ IOH.getComments bugId
    case r of
        Right cms -> return cms
        Left (IOHFetchError m) -> Servant.throwError err400 { errBody = packChars m }
        Left _    -> Servant.throwError err500

-- | Handler for getting comments submitted by a specific userId
-- @todo implement
getCommentsByUserHandler :: Int64 -- ^ unique userId to fetch comments submitted by
                         -> Handler [ApiTypes.Comment]
getCommentsByUserHandler uId = undefined


getLatestCommentsHandler :: Handler [ApiTypes.Comment]
getLatestCommentsHandler = liftIO $ IOH.getLatestComments

-- | (Init+ Only) Handler for submitting a new Comment
postCommentHandler :: AuthenticatedUser -- ^ the requesting user
                    -> ApiTypes.Comment -- ^ the comment data to insert
                    -> Handler ResponseMessage
postCommentHandler au cm = do
  unless (isUserInit au)
    (Servant.throwError err401 { errBody = "User must be Initiate role or higher" })
  r <- liftIO $ IOH.storeComment au cm
  case r of
    Right id ->
      return $ ResponseMessage 200 "OK" $ "Inserted comment with ID " ++ show id
    Left (IOHInsertError m) -> Servant.throwError err400 { errBody = packChars m }
    Left _   -> Servant.throwError err500 { errBody = "Failed to insert comment" }


-- | (Init+ Only) Handler for deleting a single Comment
deleteCommentHandler :: AuthenticatedUser -- ^ the requesting user
                    -> Int64 -- ^ the id of the comment to delete
                    -> Handler ResponseMessage
deleteCommentHandler au cId = do
  unless (isUserInit au)
    (Servant.throwError err401 { errBody = "User must be Initiate role or higher" })
  r <- liftIO $ IOH.deleteComment au cId
  case r of
    Right id ->
      return $ ResponseMessage 200 "OK" $ "Deleted comment with ID " ++ show cId
    Left (IOHInsertError m) -> Servant.throwError err400 { errBody = packChars m }
    Left _   -> Servant.throwError err500 { errBody = "Failed to delete comment" }
