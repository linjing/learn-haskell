import Data.List (tails)

combinations :: Int -> [a] ->[[a]]
combinations 0 _ = [[]]
combinations n xs  = [ y:ys | y:xs' <- tails xs, ys<- combinations (n-1) xs' ]

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n [] = error "bad args"
choose n xs = [ x:ys | x <- xs, ys <- choose (n-1) xs] 
