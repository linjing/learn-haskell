powerSet = choice (\x -> [True, False])
choice :: (a -> [Bool]) -> [a] ->[[a]]
choice _ [] = [[]]
choice f (x:xs) = [ if choose then x:ys else ys | choose <- f x, ys<- choice f xs ]

powerSet' :: [a] ->[[a]]
powerSet' [] = [[]]
powerSet' (x:xs) = [x:r | r<- powerSet' xs] ++ powerSet' xs
