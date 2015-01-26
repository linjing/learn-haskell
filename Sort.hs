insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x:y:ys
                | otherwise = y : (insert x ys)

insertionSortImpl :: Ord a => [a] -> [a] -> [a]
insertionSortImpl xs [] = xs
insertionSortImpl xs (y:ys) = insertionSortImpl (insert y xs) ys

insertionSort ::  Ord a => [a] -> [a]
insertionSort xs = insertionSortImpl [] xs


insertionSort2 :: Ord a => [a] -> [a]
insertionSort2 [] = []
insertionSort2 (x:xs) = insert x (insertionSort2 xs)

swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x1:x2:xs) | x1 > x2 = x2 : swaps (x1:xs)
                 | otherwise = x1 : swaps (x2:xs)
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
             where x' = f x

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = fix swaps xs

bubbleSort'' :: Ord a => [a] -> [a]
bubbleSort'' [] = []
bubbleSort'' xs = (bubbleSort'' initialElements) ++ [lastElement]
                 where swappedXs = swaps xs
                       initialElements = init swappedXs
                       lastElement = last swappedXs

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (l:ls) | x == l = ls
                | otherwise = l : delete x ls
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = mini : selectionSort xs'
               where mini = minimum xs
                     xs'  = delete mini xs
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort smalls) ++ [x] ++ (quickSort bigs)
              where smalls = filter (<x) xs
                    bigs   = filter (>=x) xs

filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
filterSplit _ [] = ([], [])
filterSplit f (x:xs) | f x = ((x:l), r)
                     | otherwise = (l, (x:r))
                  where (l, r) = filterSplit f xs
quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' [x] = [x]
quickSort' (x:xs) = (quickSort' smalls) ++ [x] ++ (quickSort' bigs)
              where (smalls, bigs) = filterSplit (<x) xs

split :: [a] -> ([a], [a])
split [x,y] = ([x], [y])
split xs = (l, r)
        where n = length xs `div` 2
              l = take n xs
              r = drop n xs
merge :: Ord a => [a] -> [a] -> [a]
merge [] rs = rs
merge ls [] = ls
merge (l:ls) (r:rs) | (l < r) = l : (merge ls (r:rs))
                    | otherwise = r : (merge (l:ls) rs)
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge l r
           where (ll, rr) = split xs
                 l = mergeSort ll
                 r = mergeSort rr
-- mergeSort :: Ord a => [a] -> [a]
-- mergeSort [] = []
-- mergeSort [x] = [x]
-- mergeSort xs = merge (mergeSort x1) (mergeSort x2)
--            where (x1, x2) = halve xs
--                  halve xs = (take l xs, drop l xs)
--                  l = (length xs) `div` 2
