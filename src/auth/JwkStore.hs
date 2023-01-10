-- | Functions related to storing and extracting the master JWK to disk
module JwkStore
    (
      loadJwk
    ) where
---
import Servant.Auth.Server ( generateKey )
import System.Directory ( doesFileExist )
import Crypto.JOSE.JWK ( JWK )
import qualified Data.ByteString.Lazy as L
import Data.Aeson (encode, decode, parseJSON)
import Control.Exception ( catch, IOException )
---

-- | the JWK default path to use
jwkDefaultPath :: FilePath
jwkDefaultPath = "data/jwk"

-- | Attempt to write the encoded JWK to disk
writeJwkToLocalFile :: FilePath -- ^ the location to write to
                    -> L.ByteString -- ^ encoded JWK bytestring
                    -> IO Bool -- ^ IO Success bool indicator
writeJwkToLocalFile f jwk = do
    shouldWrite <- doesFileExist f
    if shouldWrite then pure False else (do
      L.writeFile f jwk
      pure True)

-- | Attempt to read a stored JWK from disk
readJwkFromLocalFile :: FilePath -- ^ the location to read from
                     -> IO (Maybe L.ByteString)
readJwkFromLocalFile f = do
    exists <- doesFileExist f
    if exists then (do
      r <- catch (L.readFile f) (\e -> do
        putStrLn "Failed to read JWTTokenFile"
        print (e :: IOException)
        return L.empty)
      if r /= L.empty then pure $ Just r else pure Nothing) else pure Nothing

-- | Attempts to either load an existing JWK from store (disk)
-- or generates a new JWK and returns it in a tuple with the
-- first bool indicating whether or not it was read or generated 
loadJwk :: FilePath -- ^ the location to read/write from/to
        -> IO (Bool,JWK) -- ^ (Did read from disk, the JWK)
loadJwk f = do
  lKey <- readJwkFromLocalFile f
  case lKey of
    Just jwk -> do
      let result = decode jwk
      case result of
        Just k -> pure (True,k)
        Nothing -> writeNewJwk
    Nothing -> writeNewJwk
  where
    writeNewJwk = do
      newKey <- generateKey
      let wKey = encode newKey
      b <- writeJwkToLocalFile f wKey
      pure (False,newKey)
