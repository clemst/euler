main = do
    print $ res

res = head $ dropWhile (isNotDivUntil 20) [20,40..maxN]
    where maxN = product [1..10]

isNotDivUntil m n = (dropWhile (\i -> mod n i == 0) [1..m]) /= []
