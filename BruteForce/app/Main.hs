module Main (main) where
import Lib

main :: IO()
main = do
    putStrLn "Shall we help you to hash your password? <Yes/No> \n(You can also use any online coder)"
    hashPass
    putStrLn "Print your hash in SHA-1"
    hash <- getLine
    putStrLn "Please, answer our questions honestly, It will help us a lot (Answer <Yes> or <No>)"
    putStrLn "What is the lenght of your password?"
    -- putStrLn "If you don't want to answer type <No>"
    size <- getLine 
    putStrLn "Are there any special symbols (as . , - + etc.)?"
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
                        "Yes" -> print $ "Your password is: " ++ bruteForce hash (digits ++ symbols ++ alphabet ++ upperAlphabet) (read size :: Int)
                        "No" -> print $ "Your password is: " ++ bruteForce hash (digits ++ symbols ++ alphabet) (read size :: Int)
                "No" -> print $ "Your password is:" ++ bruteForce hash (digits ++ symbols) (read size :: Int)
        "No" -> do
            putStrLn "Are there any letters?"
            ans2 <- getLine
            case ans2 of
                "Yes" -> do
                    putStrLn "Are there any CAPITAL letters?"
                    ans3 <- getLine
                    case ans3 of
                        "Yes" -> print $ "Your password is: " ++ bruteForce hash (digits ++ alphabet ++ upperAlphabet) (read size :: Int)
                        "No" -> print $ "Your password is: " ++ bruteForce hash (digits ++ alphabet) (read size :: Int) -----------------------------------------
                "No" -> print $ "Your password is:" ++ bruteForce hash digits (read size :: Int)

hashPass :: IO()
hashPass = do
    choise <- getLine
    case choise of
        "Yes" -> hash
        "No" -> putStrLn "Ok, hope you have a hash"
        _ -> do
            putStrLn "Answer only <Yes> or <No>"
