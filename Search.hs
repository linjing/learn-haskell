

search :: Ord a => [a] -> a -> Bool
search [] _ = False
search as a     | m < a = search behind a
                | m > a = search front a
                | otherwise = True
        where (front, m:behind) = splitAt (length as `div` 2) as
