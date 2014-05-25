import Data.List

triangle = scanl (+) 0 [1..]

res = head $ dropWhile ( (<500) . numDivisors) triangle

numDivisors = product . map (+1) . map (length) . group . primeFactors

primeFactors n = primeFactors' n 2
  where
    primeFactors' n f
      | f*f > n        = [n]
      | n `mod` f == 0 = f : primeFactors' (n `div` f) f
      | otherwise      = primeFactors' n (f + 1)

main = print res
