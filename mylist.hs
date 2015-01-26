data List a = Nil | Cons a (List a) deriving (Eq, Show)

listToMylist Nil = []
listToMylist (Cons x xs) = x:(listToMylist xs)

mylistToList [] = Nil
mylistToList (x:xs) = Cons x (mylistToList xs)
