data Tree a = Empty | Branch a (Tree a) (Tree a)
                deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
  in [Branch 'x' left right | i <- [q..(q+r)],
                              left <- cbalTree i,
                              right <- cbalTree (n - i - 1)]

debugCbalTree :: Int -> [(Int, Int, Int, Int)]
debugCbalTree n = let (q, r) = (n -1) `quotRem` 2
  in [(n, n-1, i, n - i - 1) | i <-[q..(q+r)]]

-- mirror
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch a left right) = sameStruct left right
  where sameStruct Empty Empty = True
        sameStruct (Branch a ll lr) (Branch b rl rr) = sameStruct ll rr && sameStruct lr rl
        sameStruct _ _ = False

add :: Ord a => a -> Tree a -> Tree a
add a Empty = Branch a Empty Empty
add a t@(Branch b left right) = 
  case compare a b of
    LT -> Branch b (add a left) right
    EQ -> t
    GT -> Branch b left (add a right)

construst :: Ord a => [a] -> Tree a
construst xs = foldl (flip add) Empty xs
construst' xs = foldl (\t x -> add x t) Empty xs

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric $ cbalTree n

hbalTree :: a -> Int -> [Tree a]
hbalTree a 0 = [Empty]
hbalTree a 1 = [(Branch a Empty Empty)]
hbalTree a n = [(Branch a left right) | left <- hbalTree a (n-1), right <- hbalTree a (n-2)] 
               ++ [(Branch a left right) | left <- hbalTree a (n-2), right <- hbalTree a (n-1)]
               ++ [(Branch a left right) | left <- hbalTree a (n-1), right <- hbalTree a (n-1)]

maxNodes :: Int -> Int
maxNodes h = 2^h - 1

minHeight :: Int -> Int
minHeight nodes = ceiling $ logBase 2 $ fromIntegral (nodes+1)

minNodes :: Int -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes h = 1 + minNodes (h-1) + minNodes (h-2)

maxHeight :: Int -> Int
maxHeight nodes = (length $ takeWhile (\x -> x <= nodes) $ map minNodes [0..]) - 1

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch a left right) = 1 + countNodes left + countNodes right


hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes a nodes = filter (\t -> countNodes t == nodes) $ concatMap (\t -> hbalTree a t) [(minHeight nodes)..(maxHeight nodes)]
