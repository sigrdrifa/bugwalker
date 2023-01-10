-- | Main module of the BugWalkerLib
module BugWalkerLib
    (
      run
    ) where
---
import Data.Time
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)
---
import GameData ( runGameDataUpdate )
import BugWalkerServer ( runServerWithJWT )
import Logging (Logger (..), LogLevel (..), startLoggingThread)
import Queue (WriteQueue (..), ReadQueue (..))
---

-- | Starts the BugwalkerAPI Server with full JWT auth
run :: Int -- ^ the port to start the server on
    -> Bool -- ^ Whether or not to init the GameData db
    -> IO ()
run port doInitGameData = do
    rawLogQueue <- atomically newTQueue
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") now
    forkIO $ startLoggingThread Logging.Debug ("logs/" ++ timestamp ++ "-bugwalker.log") $ ReadQueue rawLogQueue
    let log = Logger $ WriteQueue rawLogQueue
    when doInitGameData $ GameData.runGameDataUpdate False False
    runServerWithJWT log port


