data Tree a = Empty | Branch a (Tree a) (Tree a)
                deriving (Show, Eq)

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )


getPos :: Tree (Char, (Int,Int)) -> (Int, Int)
getPos (Branch (a, (x,y)) l r) = (x,y)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch a l r) = 1 + countNodes l + countNodes r

depth :: Tree a -> Int
depth Empty = 0
depth (Branch a l r) = 1 + max (depth l) (depth r)

leftDepth :: Tree a -> Int
leftDepth Empty = 0
leftDepth (Branch a l r) = 1 + (leftDepth l)

layout65 :: Tree Char -> Tree (Char, (Int,Int))
layout65 t = helper x1 1 sep1 t
  where d = depth t
        ld = leftDepth t
        x1 = 2^(d-1) - 2^(d-ld) + 1
        sep1 = 2^(d-2)
        helper x y sep Empty = Empty
        helper x y sep (Branch a l r) = Branch (a, (x,y)) l' r'
          where l' = helper (x-sep) (y+1) (sep `div` 2) l
                r' = helper (x+sep) (y+1) (sep `div` 2) r


-- 
layout66 :: Tree a -> Tree (a, (Int,Int))
layout66 t = t'
  where (l, t', r) = helper x1 1 t
        x1 = maximum l + 1

        helper :: Int -> Int -> Tree a -> ([Int], Tree (a, (Int,Int)), [Int])
        helper x y Empty = ([], Empty, [])
        helper x y (Branch a l r) = (ll', Branch (a, (x,y)) l' r', rr')
         where (ll, l', lr) = helper (x-sep) (y+1) l
               (rl, r', rr) = helper (x+sep) (y+1) r
               sep = maximum (0:zipWith (+) lr rl) `div` 2 + 1
               ll' = 0 : overlay (map (+sep) ll) (map (subtract sep) rl)
               rr' = 0 : overlay (map (+sep) rr) (map (subtract sep) lr)

overlay :: Ord a => [a] -> [a] -> [a]
overlay [] ys = ys
overlay xs [] = xs
overlay (x:xs) (y:ys) = x : overlay xs ys
--overlay (x:xs) (y:ys) = (max x y): overlay xs ys
-- https://www.haskell.org/haskellwiki/99_questions/Solutions/66
helper66 :: Int -> Int -> Tree a -> ([Int], Tree (a, (Int,Int)), [Int])
helper66 x y Empty = ([], Empty, [])
helper66 x y (Branch a l r) = (ll', Branch (a, (x,y)) l' r', rr')
 where (ll, l', lr) = helper66 (x-sep) (y+1) l
       (rl, r', rr) = helper66 (x+sep) (y+1) r
       sep = maximum (0:zipWith (+) lr rl) `div` 2 + 1
       ll' = 0 : overlay (map (+sep) ll) (map (subtract sep) rl)
       rr' = 0 : overlay (map (+sep) rr) (map (subtract sep) lr)
