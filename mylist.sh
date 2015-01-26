data List a = Nil | Cons a (List a) deriving (Eq, Show)
lhead :: List a -> a
lhead (Nil a) = a
lhead (Cons a _) = a

listToMylist Nil = []
listToMylist (Cons x xs) = x:(listToMylist xs)

mylistToList [] = Nil
mylistToList (x:xs) = Cons x (mylistToList xs)
