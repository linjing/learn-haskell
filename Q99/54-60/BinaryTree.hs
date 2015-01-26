Tree a = EmptyTree | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

leaf a = Branch a EmptyTree EmptyTree


