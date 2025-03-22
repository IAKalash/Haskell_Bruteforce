module Lib where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString as Strict
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as C

digits = "0123456789"
alphabet = "abcdefghijklmnopqrstuvwxyz"
upperAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
symbols = "-_+=()*&^%$#@!~`'\";:[]{}/?.>,<|\\"

bruteForce :: String -> String -> Int -> String
bruteForce hash list size = check hash (permutations list size)

check :: String -> [String] -> String
check hash (x:xs) | hash == toHex (hashlazy $ C.fromStrict $ C.pack x) = x
                  | otherwise = check hash xs
check hash [] = "Hash not found :-<"

toHex :: Strict.ByteString -> String
toHex bytes = Strict.unpack bytes >>= printf "%02x"

permutations :: Eq a => [a] -> Int -> [[a]]
permutations list 1 = split list
permutations list n = add (permutations list (n - 1)) list

split :: [a] -> [[a]]
split [x] = [[x]]
split (x:xs) = [x] : split xs

add :: [[a]] -> [a] -> [[a]]
add list [x] = map (x:) list
add list (x : xs) = map (x:) list ++ add list xs

hash :: IO ()
hash = do
  putStrLn "Print your password:"
  str <- getLine
  putStrLn $ "Your hash is: " ++ toHex (hashlazy $ C.fromStrict $ C.pack str)
