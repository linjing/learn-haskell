








mygcd :: Int -> Int -> Int
mygcd x y = if mod x y == 0 then y else mygcd y (mod x y)

power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n-1)

delete :: Eq a => a ->  [a] -> [a]
delete _ [] = []
delete x (x1:xs) = if x == x1 then delete x xs else x1:(delete x xs)

