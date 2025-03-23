import Data.Word (Word32, Word8)
import Data.Bits (Bits, rotate, xor, (.&.), (.|.), complement, shiftL)
import Data.Char (ord)
import Numeric (showHex)

-- Начальные значения регистров
initH :: [Word32]
initH = [0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0]

-- Константы K для каждого раунда
k :: Int -> Word32
k t
  | t < 20  = 0x5A827999
  | t < 40  = 0x6ED9EBA1
  | t < 60  = 0x8F1BBCDC
  | otherwise = 0xCA62C1D6

-- Логические функции F
f :: Int -> Word32 -> Word32 -> Word32 -> Word32
f t b c d 
    | t < 20    = (b .&. c) .|. ((complement b) .&. d)
    | t < 40    = b `xor` c `xor` d
    | t < 60    = (b .&. c) .|. (b .&. d) .|. (c .&. d)
    | otherwise = b `xor` c `xor` d

-- Циклический сдвиг влево
rotLeft :: Int -> Word32 -> Word32
rotLeft n x = rotate x n

-- Разбиение на блоки по 64 байта
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- Дополнение сообщения
padMessage :: [Word8] -> [Word8]
padMessage msg = 
    let len = fromIntegral (length msg) * 8
        padLen = ((448 - (len + 8) `mod` 512) `mod` 512) `div` 8
        padding = replicate padLen 0
        lenBytes = reverse $ take 8 $ map fromIntegral (iterate (`div` 256) len) ++ repeat 0
    in msg ++ [0x80] ++ padding ++ lenBytes

-- Преобразование строки в список байтов
stringToBytes :: String -> [Word8]
stringToBytes = map (fromIntegral . ord)

-- Преобразование 64 байтов в 16 слов по 32 бита (big-endian)
toWords :: [Word8] -> [Word32]
toWords bytes = [ combine (take 4 $ drop (i*4) bytes) | i <- [0..15] ]
  where
    combine [a, b, c, d] = shiftL (fromIntegral a) 24 .|. shiftL (fromIntegral b) 16 .|. shiftL (fromIntegral c) 8 .|. fromIntegral d
    combine xs = combine (replicate (4 - length xs) 0 ++ xs)

-- Расширение 16 слов до 80
expandWords :: [Word32] -> [Word32]
expandWords ws = result
  where
    result = take 80 $ ws ++ go 16 ws
    go 80 _ = []  -- Останавливаемся на 80 словах
    go i prevWords =
      let newWord = rotLeft 1 (w (i-3) `xor` w (i-8) `xor` w (i-14) `xor` w (i-16))
          w j = if j < 0 || j >= length prevWords then 0 else prevWords !! j
      in newWord : go (i+1) (prevWords ++ [newWord])

-- Обработка одного блока
processBlock :: [Word32] -> [Word8] -> [Word32]
processBlock [h0, h1, h2, h3, h4] block =
  let w = expandWords $ toWords block
      (a', b', c', d', e') = foldl (step w) (h0, h1, h2, h3, h4) [0..79]
  in [h0 + a', h1 + b', h2 + c', h3 + d', h4 + e']
  where
    step w (a, b, c, d, e) t =
      let temp = rotLeft 5 a + f t b c d + e + k t + w!!t
      in (temp, a, rotLeft 30 b, c, d)

sha1 :: String -> String
sha1 input = concatMap (\x -> padHex (showHex x "")) finalHash
  where
    bytes = stringToBytes input
    padded = padMessage bytes
    blocks = chunk 64 padded
    finalHash = foldl processBlock initH blocks
    padHex s = replicate (8 - length s) '0' ++ s
    padHex2 s = replicate (2 - length s) '0' ++ s