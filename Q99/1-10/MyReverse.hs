
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' xs = rvs xs [] 
  where
    rvs [] reversed = reversed
    rvs (x:xs) reversed = rvs xs (x:reversed)
