-- | Types used for the GameData lib 
module GameDataTypes
    ( Build (..),
      SpellData (..),
      Spell (..),
      createBuild,
      toBuildString
    ) where

import Data.List.Split ( splitOn )
import Text.Read ( readMaybe )

{- Data Types -}

-- | Represents a Game build
data Build = Build {
  expV :: Int,
  major :: Int,
  minor :: Int,
  version :: Int,
  date :: String
} deriving (Show, Eq)

-- | A single Game Spell 
data Spell = Spell {
    spellId :: Int,
    spellName :: String,
    spellDesc :: String
} deriving (Eq)

instance Show Spell where
   show spell = mconcat [spellName spell
                       ," ("
                       , show $ spellId spell
                       , ")  "
                       ," - "
                       ,spellDesc spell]

-- | Collection of Game Spells for a given Build
data SpellData = SpellData {
    build :: Build,
    spells :: [Spell]
} deriving (Show, Eq)

{- Functions -}

-- | Generate a String representation of a Build
toBuildString :: Build -- ^ the build to stringify
              -> String
toBuildString b =
    show (expV b) ++ "." ++ show (major b) ++ "." ++ show (minor b)
    ++ "." ++ show (version b)

-- | Attempts to generate a Build representation of a given buildstring and date.
createBuild :: String -- ^ UTC DateTime string
            -> String -- ^ Build string 
            -> Either String Build -- ^ Right build or Left error
createBuild d bs =
    case fromBuildString bs of
        Just build -> Right build
        _ -> Left "Invalid Build String supplied"
    where
        fromBuildString s =
            case traverse readMaybe (splitOn "." s) of
                Just [e, m, n, v] -> Just (Build e m n v d)
                _ -> Nothing
