main = print res

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
res = sum $ filter even $ takeWhile (<4000000) fibs

