import Control.Applicative

data MyElem a = Single a | Multiple Int a deriving Show 

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = (takeWhile theSame xs) : (pack (dropWhile theSame xs))
  where first = head xs
        theSame x = first == x

f :: (Int, a) -> MyElem a
f x@(i, a)
  | i == 1 = Single a
  | otherwise = Multiple i a

encodeModified :: (Eq a) => [a] -> [MyElem a]
encodeModified xs = map f $ map ((,) <$> length <*> head) $ pack xs

decodeModified :: (Eq a) => [MyElem a] -> [a]
decodeModified xs = concatMap decodeHelper xs
  where
    decodeHelper (Single a) = [a]
    decodeHelper (Multiple i a) = replicate i a


encode' :: (Eq a) => [a] -> [(Int, a)]
encode' xs = foldr helper [] xs
  where
    helper :: (Eq a) => a -> [(Int, a)] -> [(Int, a)]
    helper a [] = [(1, a)]
    helper a ys@(x@(i,b):xs)
      | a == b = (i+1,b):xs
      | otherwise = (1,a):ys

encodeDirect :: (Eq a) => [a] -> [MyElem a]
encodeDirect xs = map f $ encode' xs


