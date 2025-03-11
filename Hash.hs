import Data.Bits
import Data.Char (ord)
import Data.Word (Word32)

h0 :: Word32
h0 = 0x67452301
h1 :: Word32
h1 = 0xEFCDAB89
h2 :: Word32
h2 = 0x98BADCFE
h3 :: Word32
h3 = 0x10325476
h4 :: Word32
h4 = 0xC3D2E1F0


fread :: IO()
fread = do
    putStrLn "Print your number:"
    input <- getLine
    print $ preParsing (read input :: Word32)

preParsing :: Bits a => a -> a
preParsing str = (setBit (str `shiftL` 1) 0)

