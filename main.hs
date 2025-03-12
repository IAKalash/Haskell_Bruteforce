import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString as Strict
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as C
import Data.Char (toUpper)

-- TODO: 
    --Stack overflow if size > 7 (with digits)
    --SHA1 hash
    --Unknown size
    --Otherwises for questions
    --make a project


digits :: String
digits = "0123456789"
alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"
symbols :: String
symbols = "-_+=()*&^%$#@!~`'\";:[]{}/?.>,<|\\"

start :: IO()
start = do
    putStrLn "print your hash in SHA-1"
    hash <- getLine
    putStrLn "Please, answer our questions honestly, It will help us a lot (Answer <Yes> or <No>)"
    putStrLn "What is the lenght of your password?"
    -- putStrLn "If you don't want to answer type <No>"
    size <- getLine 
    putStrLn "Are there any symbols (as . , - + etc.)?"
    ans <- getLine
    case ans of
        "Yes" -> do
            putStrLn "Are there any letters?"
            ans2 <- getLine
            case ans2 of
                "Yes" -> do
                    putStrLn "Are there any CAPITAL letters?"
                    ans3 <- getLine
                    case ans3 of
                        "Yes" -> print $ "Your password is:" ++ bruteForce hash (digits ++ symbols ++ alphabet ++ map toUpper alphabet) (read size :: Int)
                        "No" -> print $ "Your password is:" ++ bruteForce hash (digits ++ symbols ++ alphabet) (read size :: Int)
                "No" -> print $ "Your password is:" ++ bruteForce hash (digits ++ symbols) (read size :: Int)
        "No" -> do
            putStrLn "Are there any letters?"
            ans2 <- getLine
            case ans2 of
                "Yes" -> do
                    putStrLn "Are there any CAPITAL letters?"
                    ans3 <- getLine
                    case ans3 of
                        "Yes" -> print $ "Your password is:" ++ bruteForce hash (digits ++ alphabet ++ map toUpper alphabet) (read size :: Int)
                        "No" -> print $ "Your password is:" ++ bruteForce hash (digits ++ alphabet) (read size :: Int)
                "No" -> print $ "Your password is:" ++ bruteForce hash digits (read size :: Int)

bruteForce :: String -> String -> Int -> String
bruteForce hash alphabet size = check hash (permutations digits size)

check :: String -> [String] -> String
check hash (x:xs) | hash == toHex (hashlazy $ C.fromStrict $ C.pack x) = x
                  | otherwise = check hash xs
check hash [] = "Hash is no found, You've lied to us :-<"

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
