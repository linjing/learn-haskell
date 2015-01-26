factorial :: Integer -> Integer
factorial n = if n < 0 then error "n must >= 0"
  else if n == 0 then 1
  else n * factorial (n - 1)
