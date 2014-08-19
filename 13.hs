import System.IO

parseNumbers :: String -> [Integer]
parseNumbers s = map read $ lines s

main = do
    contents <- readFile "numbers.txt"
    print $ sum $ parseNumbers contents
