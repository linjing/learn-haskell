import Data.List

myGcd :: Int -> Int -> Int
myGcd a b
  | b > a = myGcd b a
  | rem a b == 0 = b
  | otherwise = myGcd b (a `mod` b)


coprime :: Int -> Int -> Bool
coprime a b = myGcd a b == 1

phi :: Int -> Int
phi a = length [ x | x<-[1..a], coprime a x ]

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors i = h:(primeFactors (i `div` h))
  where h = head [ x | x <-[1..i], myGcd x i > 1 ]

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult i = trans $ groupBy (\a b -> a == b) $ primeFactors i
  where trans = map (\a -> (head a, length a))


isPrime :: Int -> Bool
isPrime i
  | i == 1 = True
  | i == 2 = True
  | otherwise = length [ p |  p<-[2..maxP], rem i p == 0 ] == 0
  where maxP = head [ p  | p<- [1..i], p *p >= i]

primesR :: Int -> Int -> [Int]
primesR l r = filter isPrime [l..r]

goldbach :: Int -> (Int, Int)
goldbach i = head  [ (x, i - x) | x <- primesR 2 i, isPrime (i - x) ]

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList l r =  map goldbach $ filter even [ll..r]
  where
    ll = if l > 2 then l else 3
    even x = x `mod` 2 == 0
