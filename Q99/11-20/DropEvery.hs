
dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs n = (take (n-1) xs) ++ (dropEvery (drop n xs) n)
