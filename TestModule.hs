module TestModule (add, sub, month) where

add, sub :: Int -> Int -> Int
add a b = a + b
sub a b = a - b

isTwo :: Int -> Bool
isTwo n = if n == 2 then True else False

month :: Int -> Int
month n = case n of
  1 -> 31
  2 -> 28
  3 -> 31
  4 -> 30
  5 -> 31
  6 -> 30
  7 -> 31
  8 -> 31
  9 -> 30
  10 -> 31
  11 -> 30
  12 -> 31
  _ -> error "invalid month"

