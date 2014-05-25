import Data.List

primeFactors n = primeFactors' n 2
  where
    primeFactors' n f
      | f*f > n        = [n]
      | n `mod` f == 0 = f : primeFactors' (n `div` f) f
      | otherwise      = primeFactors' n (f + 1)

res (a:b:c:d:xs) | sizeEq 4 [a,b,c,d] && allDiff 4 [a,b,c,d] = a
               | otherwise = res (b : c : d : xs)
             where f = (nub . primeFactors)
                   g = length . f
                   sizeEq n l = foldl1 (&&) $ map ((==n) . g) l
                   allDiff n l = (length . concat . map f) l == n*length l

main = print $ res [1..]
