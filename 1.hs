main = do
    print $ sum $ l 1000

l :: Int -> [Int]
l n = filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1..n-1]

