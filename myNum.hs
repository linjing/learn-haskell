data MyNum = My Int
instance Eq MyNum where
  My 1 == My 1 = False
  _ == _ = True
