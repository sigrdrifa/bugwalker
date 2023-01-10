{-# LANGUAGE OverloadedStrings #-}

-- | Handlers related to GameData Asset API endpoints
module AssetApi
    (
        listBuildsHandler,
        listSpellsHandler,
        iohInitHandler
    ) where
---
import Servant ( Handler )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
---
import ApiTypes
    ( BuildT, ResponseMessage(ResponseMessage), SpellT )
import IOH ( getBuilds, getSpells )
import AuthTypes ( AuthenticatedUser(auRole) )
import GameData ( runGameDataUpdate )
import Logging (Logger, logDebug)
---

-- | Handles listing builds
listBuildsHandler :: Handler [BuildT]
listBuildsHandler = liftIO IOH.getBuilds

-- | Handles listing spells
listSpellsHandler :: Logger -> Handler [SpellT]
listSpellsHandler log = liftIO $ do
    logDebug log "ASSETS" "Listing spells..."
    IOH.getSpells

-- | (Mod Only) Handles a request to run a full GameData update
iohInitHandler :: AuthenticatedUser -- ^ the requesting user
               -> String -- ^ the source to update from (local or anything else)
               -> Handler ResponseMessage
iohInitHandler au src =
  if auRole au > 3 then do
    liftIO $ GameData.runGameDataUpdate False $ src == "local"
    return $ ResponseMessage 200 "OK" "IOH GameData init completed"
  else
    pure $ ResponseMessage 201 "NoAuth" "You are not authorised to do this."
