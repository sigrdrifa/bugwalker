-- | Contains functions needed for parsing spelldump data into ADTs.
module Parser
    ( parseSpells,
      parseSpellData,
    ) where

import Data.List.Split ( splitOn )
import Data.List ( find, isPrefixOf )
import Data.Char ( isDigit )

import GameDataTypes
    ( createBuild, Spell(Spell), SpellData(SpellData) )

-- | Attempts to extract spellId from the given SpellData dump
extractSpellId :: String -- ^ SpellData string containing the SpellId identifier
               -> Maybe Int
extractSpellId s =
    case find (isPrefixOf "(id=") $ words s of
        Just id -> Just $ read $ takeWhile isDigit $ drop 4 id
        Nothing -> Nothing

-- | Attempts to extract spellName from the given SpellData dump
extractSpellName :: String -- ^ The string containing the spellName identifier
                 -> Maybe String
extractSpellName s =
    if not (null n) then
        Just (init $ foldr (\x y -> x ++ " " ++ y) "" n)
    else
        Nothing
    where n = takeWhile (\x -> '(' /= head x) $ words s

-- | attempts to extract spellDesc from the given SpellData dump
extractSpellDescription :: [String] -- ^ SpellData dump that we can find Description in
                        -> Maybe String
extractSpellDescription s =
    case find (isPrefixOf filter) s of
        Just f -> Just $ drop (length filter) f
        _ -> Nothing
    where filter = "Description      : "

-- | Attempts to parse a valid Spell from the given SpellData dump
parseSpell :: String -- ^ Delimiter to use when splitting SpellData dump
           -> String -- ^ SpellData dump
           -> Maybe Spell
parseSpell d s =
    case extractSpellId $ head parts of
        Just spId ->
            case extractSpellName $ head parts of
                Just spName ->
                    case extractSpellDescription parts of
                        Just spDesc -> Just $ Spell spId spName spDesc
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing
    where parts = splitOn d s

-- | Attempts to parse a list of SpellData dump chunks to extract a list of Spells.
parseSpells :: String -- ^ Delimiter to use when splitting SpellData dump
            -> [String] -- ^ The SpellData Dump chunks
            -> [Spell]
parseSpells _ []         = []
parseSpells delim (x:xs) =
    case parseSpell delim x of
        Just spell -> spell : parseSpells delim xs
        _ -> parseSpells delim xs

-- | Attempts to parse a SpellData dump and generate the full SpellData representation of it
parseSpellData :: String -- ^ Delimiter to use when splitting SpellData dump
               -> String -- ^ Delimiter to use when splitting the individual Spell data
               -> String -- ^ the SpellData dump
               -> Maybe SpellData
parseSpellData delimiter spellDelim input =
    case createBuild (metadata !! 9) (metadata !! 6) of
        Right b -> Just (SpellData b (parseSpells spellDelim $ drop 1 t))
        Left s -> Nothing
    where t = splitOn delimiter input
          metadata = words $ head t
