-- problem 16

digits :: [Char] -> [Int]
digits = map read . map (\d -> [d])

main = print $ (sum . digits) (show $ 2^1000)
