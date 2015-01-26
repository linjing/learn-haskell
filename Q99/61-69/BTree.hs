data Tree a = Empty | Branch a (Tree a) (Tree a)
                deriving (Show, Eq)

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))
-- Since a "leaf" node is a branch with two empty subtrees, it can be useful to define a shorthand function:

leaf x = Branch x Empty Empty

tree1' = Branch 'a' (Branch 'b' (leaf 'd')
                                (leaf 'e'))
                    (Branch 'c' Empty
                                (Branch 'f' (leaf 'g')
                                            Empty))

-- A binary tree consisting of a root node only
tree2 = Branch 'a' Empty Empty

-- An empty binary tree
tree3 = Empty

-- A tree of integers
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)



countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch a Empty Empty) = 1
countLeaves (Branch a left right) = countLeaves left + countLeaves right

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch a l r) = leaves l ++ leaves r

internals :: Tree a -> [a]
internals Empty = []
internals (Branch a Empty Empty) = []
internals (Branch a l r) = [a] ++ internals l ++ internals r

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch a l r) 1 = [a]
atLevel (Branch a l r) n = atLevel l (n-1) ++ atLevel r (n-1)


maxNodes :: Int -> Int
maxNodes level = 2^level - 1

maxLevel :: Int -> Int
maxLevel nodes = ceiling $ logBase 2 $ fromIntegral (nodes+1)


complateBinaryTree :: Int -> Tree Char
complateBinaryTree 0 = Empty
complateBinaryTree 1 = Branch 'x' Empty Empty
complateBinaryTree n = Branch 'x' (complateBinaryTree l) (complateBinaryTree r)
  where level = maxLevel n
        maxNodesLeft = maxNodes (level-1)
        minNodesRight = maxNodes (level-2)
        l = if maxNodesLeft + minNodesRight < n - 1 then maxNodesLeft else n - 1 - minNodesRight
        r = n - 1 - l

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch a l r) = 1 + countNodes l + countNodes r

toList :: Tree a -> [[Bool]]
toList Empty = repeat [False]
toList (Branch a l r) = [True] : zipWith (++) (toList l) (toList r)

isComplateBinaryTree :: Tree a -> Bool
isComplateBinaryTree t = theSameStruct t $ complateBinaryTree $ countNodes t
  where theSameStruct Empty Empty = True
        theSameStruct (Branch a al ar) (Branch b bl br) = theSameStruct al bl && theSameStruct ar br
        theSameStruct (Branch a al ar) Empty = False
        theSameStruct Empty (Branch a al ar) = False
isComplateBinaryTree' t = all id $ take count $ concat $ toList t
  where count = countNodes t
