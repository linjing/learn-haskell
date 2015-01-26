range :: (Enum a) => a -> a -> [a]
range s e = [s..e]
range' s e
  | s > e = []
  | otherwise = [s] ++ range (succ s) e
