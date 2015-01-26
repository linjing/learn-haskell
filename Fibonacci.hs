import Data.Ratio
-- show used time
-- :set +s

fibonacci1 :: Integral a => a -> a
fibonacci1 0  = 0
fibonacci1 1  = 1
fibonacci1 n  = fibonacci1(n - 1) + fibonacci1(n - 2)
--fibonacci1 n  | n == 0 = 0
--              | n == 1 = 1
--              | n >1 = fibonacci1(n - 1) + fibonacci1(n - 2)


fibStep :: Integral a => (a, a) -> (a, a)
fibStep (u, v) = (v, u+v)
fibPair :: Integral a => a -> (a, a)
fibPair 0 = (0, 1)
fibPair n = fibStep(fibPair (n - 1))
fastFib :: Integral a => a -> a
fastFib n = fst $ fibPair n

golden :: Int -> [ Float ]
-- golden :: Fractional b => Int -> [b]
golden n = map (\(x,y) -> (fromIntegral x ::Float)/(fromIntegral y::Float)) $ take n (iterate fibStep (0,1))


fibPairs :: Integral a => a -> [(a, a)]
fibPairs n = map fibPair [1..n]
conbine :: [(a, a)] -> [(a,a,a)]
conbine ((f1, f2) : (f3, f4) : fx) = (f1,f2,f4) : conbine((f3, f4) : fx)
conbine _ = []

