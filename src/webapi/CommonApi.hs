{-# LANGUAGE OverloadedStrings #-}

-- | Handlers related to Common API endpoints
module CommonApi
    ( 
        versionHandler
    ) where
---
import Servant ( Handler )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
---
import ApiTypes ( ResponseMessage(ResponseMessage) )
---

-- | The base version of the API
apiVersion :: String
apiVersion = "0.0.1.0"

-- | Handles fetching the base version of the API
versionHandler :: Handler ResponseMessage
versionHandler = liftIO $ return $ ResponseMessage 200 "OK" apiVersion
