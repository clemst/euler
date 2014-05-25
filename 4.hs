main = print pali

nums = [i*j | i <- [100,101..999], j <- [100,101..999]]
pali = maximum $ filter (\n -> (reverse . show) n == show n) nums
