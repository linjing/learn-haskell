data Shape = C Float Float Float | S Float Float

area :: Shape -> Float
area (C r _ _) = pi * r * r
area (S h w) = h * w
