{-# LANGUAGE OverloadedStrings #-}

-- | The main gamedata module which exports functions
--   for running a full update of the gamedata by fetching data from
--   Github, parsing it and inserting it to a given db  
module GameData
    ( runGameDataUpdate
    ) where

import GameDataTypes ( SpellData )
import Fetch ( fetchSpells, fetchSpellsLocal )
import Parser ( parseSpellData )
import IOH ( initIO, storeSpellData )

-- | Attempts to fetch a spelldata dumpfile using the F
-- Fetch module and calls on the Parser module to construct
-- a SpellData object representing the fetched data.
buildSpellData :: Bool -- ^ whether or not to fetch from a local file or via HTTP
               -> String -- ^ if useLocal is True then use this path
               ->  IO (Maybe SpellData)
buildSpellData True path = do
    putStrLn $ "Fetching SpellData from local file " ++ path ++ "..."
    s <- fetchSpellsLocal path
    return $ parseSpellData "\n\nName             : " "\n" s
buildSpellData False _ = do
    putStrLn "Fetching SpellData remotely from Github..."
    s <- fetchSpells False
    return $ parseSpellData "\\r\\n\\r\\nName             : " "\\n" $ show s

-- | Helper function that calls buildSpellData and
-- writes the resulting SpellData object to a file.
buildSpellDataFile :: Bool -- ^ fetch from a local file or via HTTP
                   -> String -- ^ the path to write the file representation to 
                   -> IO ()
buildSpellDataFile useLocal path = do
    print "Parsing db.."
    o <- buildSpellData useLocal "data/monk.txt"
    print "Writing File..."
    case o of
        Just sd -> writeFile path $ show sd
        Nothing -> print "Failed to parse spelldata"
    print "Done."

-- | Main entry point of the module, attempts to do a full gamedata update
runGameDataUpdate :: Bool -- ^ should do hardInit of db
                  -> Bool -- ^ fetch from local file (true) or via http (false) 
                  -> IO ()
runGameDataUpdate hardInit useLocal = do
    putStrLn "Running gamedata update..."
    IOH.initIO hardInit
    sp <- buildSpellData useLocal "data/monk.txt"
    case sp of
        Just sd -> IOH.storeSpellData sd
        Nothing -> print "Failed to insert spell data"
    putStrLn "Finished gamedata update."



