data Tree a = Node a [Tree a]
  deriving (Eq, Show)

tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

nnodes :: Tree a -> Int
nnodes (Node a trees) = foldl (+) 1 $map nnodes trees

treeToString :: Tree Char -> String
treeToString (Node a []) = a:"^"
treeToString (Node a trees) =  [a]++ (concat $ map treeToString trees) ++ "^"


stringToTree :: String -> Tree Char
stringToTree (x:"^") = Node x []
stringToTree (x:xs) = Node x ts
  where fig = (\x -> if x == '^' then -1 else 1)
        z = map fst $ filter (( == 0) . snd ) $ zip [0..] $ scanl (+) 0 $ map fig xs
        ts = map (stringToTree . uncurry (sub xs)) $ zip (init z) (tail z)
        sub s a b = take (b - a) $ drop a s
--(^:xs)
--(x:'^':xs) = (Node x []), xs
--(x:y:xs) 

-- TODO 70,73 use Monad

ipl :: Tree Char -> Int
ipl t = helper t 1
  where helper (Node a trees) n = n * (length trees) + (foldl (+) 0 $ map (\t -> helper t (n+1)) trees)


bottom_up :: Tree Char -> String
bottom_up (Node a trees) = foldr (++) [a] $ map bottom_up trees 
