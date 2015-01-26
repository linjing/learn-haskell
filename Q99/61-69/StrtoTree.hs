data Tree a = Empty | Branch a (Tree a) (Tree a)
                deriving (Show, Eq)

split :: [Char] -> Int ->  ([Char], [Char]) -> ([Char], [Char])
split [] _ c = c
split (x:xs) i (l, r)
  | x == ',' = if i == 0 then (l, xs) else split xs (i-1) (l ++ [x], r)
  | x == '(' = split xs (i+1) (l ++ [x], r)
  | otherwise = split xs i (l ++ [x], r)


stringToTree :: [Char] -> Tree Char
stringToTree [] = Empty
stringToTree xs = helper xs Empty
  where helper :: [Char] -> Tree Char -> Tree Char
        helper [] Empty = (Empty)
        helper [] t@(Branch a l r) = t
        helper (x:xs) Empty = helper xs (Branch x Empty Empty)-- ((Empty, Empty), Branch x Empty Empty)
        helper (x:xs) t@(Branch a l r)
          | x == '(' = if last xs /= ')'
                       then 
                         error "() error"
                      else
                         let xs' = init xs
                             (ls, rs) = split xs' 0 ([], [])
                         in Branch a (helper ls l) (helper rs r)
          | otherwise = error (x:xs ++ show t)

treeToString :: Tree Char -> [Char]
treeToString Empty = ""
treeToString (Branch a Empty Empty) = [a]
treeToString (Branch a l r) = [a] ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

treeToPreorder :: Tree Char -> String
treeToPreorder Empty = []
treeToPreorder (Branch a l r) = [a] ++ treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree Char -> String
treeToInorder Empty = []
treeToInorder (Branch a l r) = treeToInorder l ++ [a] ++ treeToInorder r


preToTree :: String -> Tree Char
preToTree "" = Empty
preToTree (x:xs) = Branch x  (preToTree xs) Empty

preInToTree :: String -> String -> Tree Char
preInToTree [] [] = Empty
preInToTree pre@(x:xs) inorder = Branch x (preInToTree pl lstr) (preInToTree pr rstr)
  where (lstr, _:rstr) = break (== x) inorder
        (pl, pr) = splitAt (length lstr) xs

preInToTreee :: Monad m => String -> String -> m (Tree Char)
preInToTreee [] [] = return Empty
preInToTreee p@(x:xs) i = do (il, _:ir) <- return $ break (== x) i
                             (pl, pr) <- return $ splitAt (length il) xs
                             l <- preInToTreee pl il
                             r <- preInToTreee pr ir
                             return $ Branch x l r
preInToTreee p i  = fail $ show p ++ "||" ++ show i

tree2ds :: Tree Char -> String
tree2ds Empty = ['.']
tree2ds (Branch a l r) = [a] ++ tree2ds l ++ tree2ds r

ds2tree :: String -> (String, Tree Char)
ds2tree ('.':xs) = (xs, Empty)
ds2tree (x:xs) = (left2, (Branch x l r))
  where (left, l) = ds2tree xs
        (left2, r) = ds2tree left

testStr = "xy..z0..."
