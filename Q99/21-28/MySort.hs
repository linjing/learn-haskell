import Data.List
import Data.Ord
import Data.Function

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = lsort lt ++ [x] ++ lsort gt
  where lt = filter (\str -> length str < length x) xs
        gt = filter (\str -> length str >= length x) xs
lsort' = sortBy (\a b -> compare (length a) (length b))
lsort'' = sortBy (compare `on` length)



lfsort :: [String] -> [String]
lfsort lists =  concat $ lsort  $ groupBy equalLength $ lsort lists
  where equalLength  xs ys = length xs == length ys
