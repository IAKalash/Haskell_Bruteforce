import SHA1
import Text.Regex.Posix ( (=~) )

import Control.Concurrent
import Control.DeepSeq
import Control.Parallel.Strategies


-- TODO: 
    --Digital only password as a 10-core increment !!!!!!!!!!!!!!!!!!!!!!!
    --Stack overflow if size > 7 (with digits) (maybe $!)
        --digits - up to 8 (if password starts from 1)
        --digi-letters - up to 5
    --Otherwises for questions
    --make a project

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
bruteForce hash list size = check hash (permutations list size)

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
digitalPassword hash n = do
    let res = bruteForce hash digits n
    case res of
        "Hash not found :-<" -> digitalPassword hash (n + 1)
        _ -> res

ifHash :: IO()
ifHash = do
    choise <- getLine
    case choise of
        "Yes" -> makeHash
        "No" -> putStrLn "Ok, hope you have a hash"
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
    case choise of
        "No" -> do
            putStrLn "Ok, bruteforcing digital passwords..."
            print $ "Your password is:" ++ digitalPassword hash 1
        "Yes" -> do
            putStrLn "What is the lenght of your password?"
            size <- pasSize
            putStrLn "Are there any special symbols (as . , - + etc.)?"
            questionsPt2 hash size
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions hash

pasSize :: IO String
pasSize = do
    size <- getLine
    if size =~ "[123456789]+"
        then return size
        else do
            putStrLn "Incorrect or too big size"
            putStrLn "print a number"
            pasSize

questionsPt2 :: String -> String -> IO()
questionsPt2 hash size = do
    ans <- getLine
    case ans of
        "Yes" -> do
            putStrLn "Are there any letters?"
            questions21 hash size
        "No" -> do
            putStrLn "Are there any letters?"
            questions22 hash size
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questionsPt2 hash size

questions21 :: String -> String -> IO()
questions21 hash size = do
    ans2 <- getLine
    case ans2 of
        "Yes" -> do
            putStrLn "Are there any CAPITAL letters?"
            questions211 hash size
        "No" -> print $ "Your password is:" ++ bruteForce hash (digits ++ symbols) (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions21 hash size

questions22 :: String -> String -> IO()
questions22 hash size = do
    ans2 <- getLine
    case ans2 of
        "Yes" -> do
            putStrLn "Are there any CAPITAL letters?"
            questions221 hash size
        "No" -> print $ "Your password is: " ++ bruteForce hash digits (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions22 hash size

questions211 :: String -> String -> IO()
questions211 hash size = do
    ans3 <- getLine
    case ans3 of
        "Yes" -> print $ "Your password is: " ++ bruteForce hash (digits ++ symbols ++ alphabet ++ upperAlphabet) (read size :: Int)
        "No" -> print $ "Your password is: " ++ bruteForce hash (digits ++ symbols ++ alphabet) (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions211 hash size

questions221 :: String -> String -> IO()
questions221 hash size = do
    ans3 <- getLine
    case ans3 of
        "Yes" -> print $ "Your password is: " ++ bruteForce hash (digits ++ alphabet ++ upperAlphabet) (read size :: Int)
        "No" -> print $ "Your password is: " ++ bruteForce hash (digits ++ alphabet) (read size :: Int)
        _ -> do
            putStrLn "Answer <Yes> or <No> (without brackets)"
            questions221 hash size
