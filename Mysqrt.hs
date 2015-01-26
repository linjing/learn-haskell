-- TODO why
fix :: (t -> t -> Bool) -> (t -> t) -> t -> t
fix c f x | c x (f x) = x
          | otherwise = fix c f (f x)

newton :: Fractional a => a -> a -> a
newton c t = (c/t + t) / 2.0

mysqrt :: Double -> Double
mysqrt c = fix (\a b -> abs (a - b) < 0.00000001) (newton c) c
