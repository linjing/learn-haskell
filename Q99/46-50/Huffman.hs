import Data.Ord
import Data.Function
import Data.List

data HTree = Node Char Int | Top Int HTree HTree 
  deriving Show


getWeight :: HTree -> Int
getWeight (Node c i) = i
getWeight (Top i tl tr) = i

toNode :: (Char, Int) -> HTree
toNode (c,i) = Node c i


huffman :: [HTree] -> [HTree]
huffman xs = sortBy (compare `on` getWeight) $ xs

huffmanTree :: [HTree] -> HTree
huffmanTree [] = error "empty tree"
huffmanTree (x:[]) = x
huffmanTree (x:y:xs) = huffmanTree (huffman ((Top newWeight x y):xs))
  where newWeight = (getWeight x) + (getWeight y)

encode :: HTree -> String -> [(Char, String)]
encode (Node a i) prefix = [(a, prefix)]
encode (Top i tl tr) prefix =  (encodeL tl prefix ++ encodeR tr prefix)

encodeL :: HTree -> String -> [(Char, String)]
encodeL tree prefix = encode tree (prefix ++ "0")

encodeR :: HTree -> String -> [(Char, String)]
encodeR tree prefix = encode tree (prefix ++ "1")

callHuffman = (\t -> encode t "") $ huffmanTree $ huffman $ map toNode [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
