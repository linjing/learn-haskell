myslice :: [a] -> Int -> Int -> [a]
myslice xs i j = drop i $ take j xs

slice :: [a] -> Int -> Int -> Maybe [a]
slice xs i j
  | i > j = Nothing
  | length xs < j = Nothing
  | otherwise = Just (drop i $ take j xs)
