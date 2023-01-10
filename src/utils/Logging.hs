{-# LANGUAGE FlexibleInstances #-}

-- | Provides threadsafe logging using a TQueue,
-- should run on a dedicated logging thread
module Logging
  ( startLoggingThread
  , Logger (..)
  , LogLevel (..)
  , logDebug
  , logError
  , logWarning
  , logInfo
  ) where
---
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Text as T
import Data.Time
import System.IO
import Text.Printf
---
import Queue
---
  --
-- | The logging context to be passed around for enqueuing
-- log entries
data Logger = Logger
   { lcLogQueue :: !(WriteQueue LogEntry)
   }

data LogLevel = Debug | Info | Warning | Error | All
  deriving(Eq, Show, Ord)

instance Loggable Logger where
  logEntry lc = logEntry $ lcLogQueue lc

data LogEntry = LogEntry
  { logEntryLevel :: LogLevel
  , logEntryTag :: T.Text
  , logEntryText :: T.Text
  } deriving (Eq, Show)

class Loggable l where
  logEntry :: l -> LogEntry -> IO ()

instance Loggable (WriteQueue LogEntry) where
  logEntry logging = atomically . writeQueue logging

instance Loggable Handle where
  logEntry handle (LogEntry lvl tag text) = do
    now <- getCurrentTime
    let timestamp =
          formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") now
    hPrintf handle "[%s] [%s] [%s] %s\n" timestamp (show lvl) tag text

-- | Log an error message
logError :: (Loggable l) => l -- ^ The logger to use
         -> T.Text -- ^ The sender or module
         -> T.Text -- ^ The message to log
         -> IO ()
logError l sender msg = logEntry l $ LogEntry Error sender msg


-- | Log an info message
logInfo :: (Loggable l) => l -- ^ The logger to use
         -> T.Text -- ^ The sender or module
         -> T.Text -- ^ The message to log
         -> IO ()
logInfo l sender msg = logEntry l $ LogEntry Info sender msg

-- | Log a warning message
logWarning :: (Loggable l) => l -- ^ The logger to use
         -> T.Text -- ^ The sender or module
         -> T.Text -- ^ The message to log
         -> IO ()
logWarning l sender msg = logEntry l $ LogEntry Warning sender msg

-- | Log a debug message
logDebug :: (Loggable l) => l -- ^ The logger to use
         -> T.Text -- ^ The sender or module
         -> T.Text -- ^ The message to log
         -> IO ()
logDebug l sender msg = logEntry l $ LogEntry Debug sender msg

-- | The main logging thread entry point, should be run in forkIO
startLoggingThread :: LogLevel -- ^ The minimum log level for messages to be logged 
                   -> FilePath -- ^ path of the file to log to
                   -> ReadQueue LogEntry -- ^ readqueue to dequeue entries from
                   -> IO ()
startLoggingThread logLevel logFilePath messageQueue = do
        putStrLn $ "Logging to " ++ logFilePath
        withFile logFilePath AppendMode loop
        where
        loop logHandle = do
         threadDelay 15000
         messages <- atomically $ flushQueue messageQueue
         let toLog = filter (\(LogEntry l _ _) -> l >= logLevel) messages
         mapM_ (\(LogEntry _ tag msg) -> putStrLn $ T.unpack msg) toLog
         mapM_ (logEntry logHandle) toLog
         hFlush logHandle
         loop logHandle

