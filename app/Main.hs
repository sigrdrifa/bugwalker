module Main where

---
import System.Environment
---
import BugWalkerLib
---

data Flag
    = PortFlag Int
    | InitDbFlag
    deriving (Show)

parseRunArgs :: [String] -> [Flag]
parseRunArgs [] = []
parseRunArgs [x] = case x of
    "--initDb" -> [InitDbFlag]
    _          -> []
parseRunArgs ["-p",p] = [PortFlag $ read p]
parseRunArgs ("-p":p:xs) = PortFlag (read p):parseRunArgs xs
parseRunArgs ("--initDb":xs) = InitDbFlag:parseRunArgs xs

getRunConfig :: [Flag] -> (Int, Bool)
getRunConfig []                       = (8080, False)
getRunConfig [PortFlag p]             = (p, False)
getRunConfig [InitDbFlag]             = (8080, True)
getRunConfig [PortFlag p, InitDbFlag] = (p, True)
getRunConfig [InitDbFlag, PortFlag p] = (p, True)


main :: IO ()
main = do
    t <- getArgs
    let (port,initDb) = getRunConfig $ parseRunArgs t
    BugWalkerLib.run port initDb

