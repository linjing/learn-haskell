gen :: Int -> Int -> [[Int]]
gen 0 j = [[]]
gen i j = [x: xs | x <- [1..j], xs <- gen (i-1) j]

queens :: Int -> [[Int]]
queens n = [ xs | xs <- gen n n, isOK xs ]
  where isOK xs = theSamePos xs && lt2rb xs && rt2lb xs
        theSamePos xs@(b:[]) = True
        theSamePos xs@(b:bs) = (0 == (length $ filter (== b) bs)) && theSamePos bs
        lt2rb xs = theSamePos $ zipWith (-) [1..] xs
        rt2lb xs = theSamePos $ zipWith (+) [1..] xs


