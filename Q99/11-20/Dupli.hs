
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x, x] ++ dupli xs


repliHelper :: a -> Int -> [a]
repliHelper a n
  | n < 1 = []
  | n == 1 = [a]
  | otherwise = [a] ++ repliHelper a (n-1)

repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> repliHelper x n) xs
