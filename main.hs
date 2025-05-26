import SHA1
import Text.Regex.Posix ( (=~) )
import Control.Parallel.Strategies
import Data.Char (toLower)

-- ghc -O2 -threaded main.hs +RTS -N10
-- ./main.exe +RTS -N10

digits = "0123456789"
alphabet = "abcdefghijklmnopqrstuvwxyz"
upperAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
symbols = "-_+=()*&^%$#@!~`'\";:[]{}/?.>,<|\\"

main :: IO()
main = do
    putStrLn "Shall we help you to hash your password? <Yes/No> \n(You can also use any online coder)"
    ifHash
    putStrLn "Print your hash in SHA-1"
    hash <- checkHash
    putStrLn "Please, answer our questions honestly, It will help us a lot (Answer <Yes> or <No>)"
    putStrLn "If you don't want to answer type <No> and we'll bruteforce only digital passwords"
    questions hash

bruteForce :: String -> String -> Int -> String
bruteForce hash list size = 
    let num = len list ^ size
        partitions = chunk (num `div` 10) (permutations list size)
        results = map (check hash) partitions `using` parList rdeepseq
    in case filter (/= "Hash not found :-<") results of
        (found:_) -> found
        [] -> "Hash not found :-<"

check :: String -> [String] -> String
check hash (x:xs) | hash == sha1 x = x
                  | otherwise = check hash xs
check hash [] = "Hash not found :-<"

len :: [a] -> Int
len = foldr (\ x -> (+) 1) 0

permutations :: Eq a => [a] -> Int -> [[a]]
permutations list 0 = []
permutations list 1 = split list
permutations list n = add (permutations list (n - 1)) list

split :: [a] -> [[a]]
split [x] = [[x]]
split (x:xs) = [x] : split xs

add :: [[a]] -> [a] -> [[a]]
add list [x] = map (x:) list
add list (x : xs) = map (x:) list ++ add list xs

digitalPassword :: String -> Int -> String
digitalPassword hash 1 | check hash (permutations digits 1) /= "Hash not found :-<" = check hash (permutations digits 1)
                       | otherwise = digitalPassword hash 2
digitalPassword hash n = do
    let res = bruteForce hash digits n
    case res of
        "Hash not found :-<" -> digitalPassword hash (n + 1)
        _ -> res

ifHash :: IO()
ifHash = do
    choise <- getLine
    case map toLower choise of
        "yes" -> makeHash
        "no" -> putStrLn "Ok, hope you have a hash"
        _ -> do putStrLn "Please, answer <Yes> or <No>"
                ifHash

makeHash :: IO ()
makeHash = do
  putStrLn "Print your password:"
  str <- getLine
  putStrLn $ "Your hash is: " ++ sha1 str

checkHash :: IO String
checkHash = do
    hash <- getLine
    if hash =~ "[a-f0123456789]{40}"
        then return hash
        else do
            putStrLn "Incorrect hash"
            putStrLn "Your SHA-1 hash must consist only of letters a-f and digits"
            putStrLn "Here is an example: 6216f8a75fd5bb3d5f22b6f9958cdede3fc086c2"
            checkHash

questions :: String -> IO()
questions hash = do
    choise <- getLine
    case map toLower choise of
        "no" -> do
            putStrLn "Ok, bruteforcing digital passwords..."
            print $ "Your password is: " ++ digitalPassword hash 1
        "yes" -> do
            putStrLn "What is the lenght of your password?"
            size <- pasSize
            putStrLn "Are there any digits?"
            questionsPt1 hash size
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions hash

questionsPt1 :: String -> String -> IO()
questionsPt1 hash size = do
    ans <- getLine
    case map toLower ans of
        "yes" -> do
            putStrLn "Are there any special symbols (as . , - + etc.)?"
            questionsPt2 hash size
        "no" -> do
            putStrLn "Are there any special symbols (as . , - + etc.)?"
            questions0 hash size
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questionsPt1 hash size

questions0 :: String -> String -> IO()
questions0 hash size = do
    ans <- getLine
    case map toLower ans of
        "yes" -> do
            putStrLn "Are there any letters?"
            questions01 hash size
        "no" -> do
            putStrLn "Are there any CAPITAL letters?"
            questions02 hash size
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions0 hash size

questions02 :: String -> String -> IO()
questions02 hash size = do
    ans <- getLine
    case map toLower ans of
        "yes" -> print $ "Your password is: " ++ bruteForce hash (alphabet ++ upperAlphabet) (read size :: Int)
        "no" -> print $ "Your password is: " ++ bruteForce hash alphabet (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions02 hash size

questions01 :: String -> String -> IO()
questions01 hash size = do
    ans <- getLine
    case map toLower ans of
        "yes" -> do
            putStrLn "Are there any CAPITAL letters?"
            questions010 hash size
        "no" -> print $ "Your password is: " ++ bruteForce hash symbols (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions01 hash size

questions010 :: String -> String -> IO()
questions010 hash size = do
    ans <- getLine
    case map toLower ans of
        "yes" -> print $ "Your password is: " ++ bruteForce hash (alphabet ++ upperAlphabet ++symbols) (read size :: Int)
        "no" -> print $ "Your password is: " ++ bruteForce hash (alphabet ++ symbols) (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions010 hash size

pasSize :: IO String
pasSize = do
    size <- getLine
    if size =~ "[123456789][0123456789]*"
        then return size
        else do
            putStrLn "Incorrect size"
            putStrLn "print a number"
            pasSize

questionsPt2 :: String -> String -> IO()
questionsPt2 hash size = do
    ans <- getLine
    case map toLower ans of
        "yes" -> do
            putStrLn "Are there any letters?"
            questions21 hash size
        "no" -> do
            putStrLn "Are there any letters?"
            questions22 hash size
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questionsPt2 hash size

questions21 :: String -> String -> IO()
questions21 hash size = do
    ans2 <- getLine
    case map toLower ans2 of
        "yes" -> do
            putStrLn "Are there any CAPITAL letters?"
            questions211 hash size
        "no" -> print $ "Your password is:" ++ bruteForce hash (digits ++ symbols) (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions21 hash size

questions22 :: String -> String -> IO()
questions22 hash size = do
    ans2 <- getLine
    case map toLower ans2 of
        "yes" -> do
            putStrLn "Are there any CAPITAL letters?"
            questions221 hash size
        "no" -> print $ "Your password is: " ++ bruteForce hash digits (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions22 hash size

questions211 :: String -> String -> IO()
questions211 hash size = do
    ans3 <- getLine
    case map toLower ans3 of
        "yes" -> print $ "Your password is: " ++ bruteForce hash (digits ++ symbols ++ alphabet ++ upperAlphabet) (read size :: Int)
        "no" -> print $ "Your password is: " ++ bruteForce hash (digits ++ symbols ++ alphabet) (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions211 hash size

questions221 :: String -> String -> IO()
questions221 hash size = do
    ans3 <- getLine
    case map toLower ans3 of
        "yes" -> print $ "Your password is: " ++ bruteForce hash (digits ++ alphabet ++ upperAlphabet) (read size :: Int)
        "no" -> print $ "Your password is: " ++ bruteForce hash (digits ++ alphabet) (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions221 hash size
