import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import System.Process (system)
import Text.Printf (printf)
import Data.ByteString (fromStrict)
import qualified Data.ByteString.Char8 as C

hashFile :: FilePath -> IO Strict.ByteString
hashFile = fmap hashlazy . Lazy.readFile 

toHex :: Strict.ByteString -> String
toHex bytes = Strict.unpack bytes >>= printf "%02x"


--WORKING SHA1:
test :: FilePath -> IO ()
test path = do
  hashFile path >>= putStrLn . toHex
  return ()


--WORKING SHA1 with reading:
main :: IO ()
main = do
  str <- getLine
  putStrLn . toHex $ hashlazy $ fromStrict $ C.pack str
  return ()