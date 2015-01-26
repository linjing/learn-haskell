rotate :: [a] -> Int -> [a]
rotate xs@(x:os) n
  | n < 0 = rotate xs (length xs + n)
  | n > length xs = rotate xs (n - length xs)
  | n == 0 = xs
  | otherwise = rotate (os++[x]) (n-1)
