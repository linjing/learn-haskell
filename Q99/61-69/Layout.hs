data Tree a = Empty | Branch a (Tree a) (Tree a)
                deriving (Show, Eq)

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )


countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch a l r) = 1 + countNodes l + countNodes r

leftCounts :: Tree a -> Int -> Int
leftCounts Empty n = n
leftCounts (Branch a l r) n = n + countNodes l

-- 先序
layoutH :: Tree Char -> Int -> Tree (Char, Int)
layoutH  Empty n = Empty
layoutH (Branch a l r) n = Branch (a, n+1) (layoutH l h) (layoutH r h)
  where h = n + 1

-- 中序
layoutX :: Tree Char -> Tree (Char, Int)
layoutX Empty = Empty
layoutX t = lX2 t 0
  where lX2 Empty n = Empty
        lX2 (Branch aa ll rr) n = Branch (aa, (countNodes ll) + 1 + n) (lX2 ll n) (lX2 rr  ((countNodes ll) + 1 + n))

layout :: Tree Char -> Tree (Char, (Int, Int))
layout t = fst $ helper 1 1 t
  where helper x y Empty = (Empty, x)
        helper x y (Branch a l r) = (Branch (a, (x',y)) l' r', x'')
          where (l', x') = helper x (y+1) l
                (r', x'') = helper (x'+1) (y+1) r
