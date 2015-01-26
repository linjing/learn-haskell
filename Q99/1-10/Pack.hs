
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = (takeWhile theSame xs) : (pack (dropWhile theSame xs))
  where first = head xs
        theSame x = first == x

