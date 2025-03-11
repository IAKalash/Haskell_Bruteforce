import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import Data.Char
import Data.Serialize
import GHC.Word

f :: String -> IO()
f str = print $ show (SHA1.hash (BS.pack $ map fromIntegral $ list str []))

hash :: IO ()
hash = do
    putStrLn "Type a string to hash with SHA-1 algorythm:"
    str <- BS.getLine
    print $ show $ SHA1.hash str

meyn = print $ SHA1.hash (BS.pack $ head $ perms' [1,2,3,4,5])


perms' :: Eq a => [a] -> [[a]]
perms' [] = [[]]
perms' (x:xs) = [y:ys | y <- x:xs, ys <- perms' (f y (x:xs))]
                                                            where f :: Eq a => a -> [a] -> [a]
                                                                  f n (x:xs) | n == x = xs
                                                                             | otherwise = x: (f n xs)

list :: String -> [Int] -> [Int]
list str xs = foldr (\ s -> (:) (ord s - 48)) xs str
