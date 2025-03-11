nums = "1234567890"

start :: IO()
start = do
    putStrLn "print your hash in SHA-1"
    --only for 5-number passwords now
    hash <- getLine
    print $ bruteForce hash nums 5

bruteForce :: String -> String -> Int -> String
bruteForce hash nums size = undefined

permutations :: Eq a => [a] -> Int -> [[a]]
permutations list 1 = split list
permutations list n = undefined -- permutations list (n - 1)

split :: [a] -> [[a]]
split [x] = [[x]]
split (x:xs) = [x] : split xs

add :: [a] -> [a] -> [a]
add list [] = list
add list (x : xs) = undefined
