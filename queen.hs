positions :: Int -> Int -> [[Int]]
positions 0 n = [[]]
positions k n = [x:xs | x<-[1..n], xs <- positions (k-1) n]

noSameRow :: [Int] -> Bool
noSameRow [] = True
noSameRow (x:xs) = (not $ elem x xs) && (noSameRow xs)

noSameDiagonal :: [Int] -> Bool
noSameDiagonal [] = True
noSameDiagonal xs@(x:xs') = and [ (abs (i1 - i)) /=(abs (p1 -p)) | (i, p) <- ip ] && noSameDiagonal xs'
                      where (i1, p1) : ip = zip [1..] xs

queens n = [ xs | xs <- positions n n, noSameDiagonal xs, noSameRow xs]

isSafe :: Int -> [Int] -> Bool
isSafe p ps = not ((elem p ps) || (sameDiag p ps))
  where sameDiag p ps = any (\(dist, q) -> abs (p-q) == dist) $ zip [1..] ps
positions' :: Int -> Int -> [[Int]]
positions' 0 n = [[]]
positions' k n = [p:ps | ps<-positions' (k-1) n, p<-[1..n], isSafe p ps]
queen' :: Int -> [[Int]]
queen' n = positions' n n
