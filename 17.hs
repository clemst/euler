-- problem 17

d = [(1,"one"),
     (2,"two"),
     (3,"three"),
     (4,"four"),
     (5,"five"),
     (6,"six"),
     (7,"seven"),
     (8,"eight"),
     (9,"nine"),
     (10,"ten"),
     (11,"eleven"),
     (12,"twelve"),
     (13,"thirteen"),
     (14,"fourteen"),
     (15,"fifteen"),
     (16,"sixteen"),
     (17,"seventeen"),
     (18,"eighteen"),
     (19,"nineteen"),
     (20,"twenty"),
     (30,"thirty"),
     (40,"forty"),
     (50,"fifty"),
     (60,"sixty"),
     (70,"seventy"),
     (80,"eighty"),
     (90,"ninety"),    
     (100,"one hundred"),
     (200,"two hundred"),
     (300,"three hundred"),
     (400,"four hundred"),
     (500,"five hundred"),
     (600,"six hundred"),
     (700,"seven hundred"),
     (800,"eight hundred"),
     (900,"nine hundred"),
     (1000, "one thousand")]

wordify n = case lookup n d of
    Just w -> w
    Nothing -> if n > 100 then  (wordify $ (n `quot` 100)*100) ++ " and " ++ wordify (n `mod` 100)
                          else (wordify $ (n `quot` 10)*10) ++ " " ++ wordify (n `mod` 10)
        
    
main = print $ sum $ map (length . removeSpace . wordify) [1..1000]
    where removeSpace s = filter (/=' ') s
