transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose xss = [ head xs | xs <- xss] : (transpose $ map tail xss)

infixl 5 |*|
(|*|) :: Num a => [[a]] -> [[a]] -> [[a]]
(|*|) a b = [ [sum $ zipWith (*) ar br | br <- transpose b ] | ar <- a]

