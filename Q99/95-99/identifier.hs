import Data.Char

identifier :: String -> Bool
identifier [] = False
identifier (x:xs) = isAlpha x && helper x xs
  where
    helper c [] = isAlphaNum c
    helper c cs
      | c == '-' = if (isAlphaNum $ head cs)
                   then helper (head cs) (tail cs)
                   else False
      | otherwise = if (isAlphaNum $ head cs) || ('-' == head cs)
                    then helper (head cs) (tail cs)
                    else False
