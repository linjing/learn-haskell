
splitHelper :: ([a], Int, [a]) -> a -> ([a], Int, [a])
splitHelper (ls, n, rs) x
  | n == 0 = (ls, 0, rs ++ [x])
  | otherwise = (ls ++ [x], n - 1, rs)

split :: [a] -> Int -> [[a]]
split xs n = [take n xs, drop n xs]
split' xs n = [a, c]
  where (a, b, c) = foldl splitHelper ([], n, []) xs

