import Prelude hiding ((/=), (==), not, and, or, (&&), (||))


(==) :: Bool -> Bool -> Bool
(==) True True   = True
(==) False False = True
(==) _  _        = False

not :: Bool -> Bool
not True = False
not False = True

not' = (==False)

xor, and, or :: Bool -> Bool -> Bool
xor b1 b2    = not $ b1 == b2
and True b2  = b2
and False _  = False
or  False b1 = b1
or  True _   = True

(||) = or
(&&) = and
(/=) = xor

infixr 4 ==
infixr 4 /=
infixl 3 &&
infixl 2 ||

{-
*Main> :l Boolean
[1 of 1] Compiling Main             ( Boolean.hs, interpreted )
Ok, modules loaded: Main.
*Main> (||) True True
True
-}
