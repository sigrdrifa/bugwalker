{-# LANGUAGE OverloadedStrings #-}

-- | Contains functions needed for fetching external GameData via HTTP
module Fetch
    ( fetchSpells,
      fetchSpellsLocal
    ) where
---
import Network.HTTP.Conduit ( simpleHttp )
import qualified Data.ByteString.Lazy as L
---

-- | The default url to fetch SpellData from
monkSpellsUrl :: String
monkSpellsUrl = "https://raw.githubusercontent.com/simulationcraft/simc/shadowlands/SpellDataDump/monk.txt"

-- | The default url to fetch PTR SpellData from
monkSpellsPtrUrl :: String
monkSpellsPtrUrl = "https://raw.githubusercontent.com/simulationcraft/simc/shadowlands/SpellDataDump/monk_ptr.txt"

-- | Makes a HTTP GET request to fetch the GameData spell dump
-- and returns the result
fetchSpells :: Bool -- ^ use PTR
            -> IO L.ByteString
fetchSpells True = simpleHttp monkSpellsPtrUrl
fetchSpells False = simpleHttp monkSpellsUrl

-- | Fetches a local GameData spell dump (for testing)
fetchSpellsLocal :: String -> IO String
fetchSpellsLocal path = do
    readFile path

