import Data.List

data Graph a = Graph [a] [(a, a)]
  deriving (Show, Eq)

data Adj a = Adj [(a, [a])]
  deriving (Show, Eq)

data Friendly a = Edge [(a, a)]
  deriving (Show, Eq)



graphToAdj :: (Eq a) => Graph a -> Adj a
graphToAdj (Graph [] _) = Adj []
graphToAdj (Graph (x:xs) abs) = Adj ([(x, es)] ++ xs2)
  where 
-- es = concatMap (\(a,b) -> f x (a,b) ) abs
        es = abs >>= f
        Adj xs2 = graphToAdj (Graph xs abs)
--        f x (a,b)
        f (a,b)
            | x == a = [b]
            | x == b = [a]
            | otherwise = []

adjToGraph :: (Eq a) => Adj a -> Graph a
adjToGraph (Adj []) = Graph [] [] 
adjToGraph (Adj ((x, ys):xs)) = Graph (x:xs2) (ns ++ ns2)
                         where 
                              ns = map (\y -> (x, y)) ys
                              xs1 = map (\(x1,ys1) -> (x1, filter (/=x) ys1)) xs
                              Graph xs2 ns2 = adjToGraph $ Adj xs1

graphToFri :: (Eq a) => Graph a -> Friendly a
graphToFri (Graph [] _) = Edge []
graphToFri (Graph xs es) = Edge $ es ++ zip g g
  where g = filter (\x -> all (\(a,b) -> x /= a && x /=b) es) xs

friToGraph :: (Eq a) => Friendly a -> Graph a
friToGraph (Edge []) = Graph [] []
friToGraph (Edge (n:ns)) = Graph (xxxx ++ xs1) (yyyy++es1)
  where Graph xs1 es1 = friToGraph (Edge ns)
        (a,b) = n
        xxxx = if a == b then [a] else
                ((if elem a xs1 then [] else [a]) ++ (if elem b xs1 then [] else [b]))
        yyyy = if a == b then [] else [n]

t1 = graphToAdj $ Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
t2 = adjToGraph t1
t3 = graphToFri t2
t4 = friToGraph t3

adjToFri :: (Eq a) => Adj a -> Friendly a
adjToFri = graphToFri . adjToGraph
 
friToAdj :: (Eq a) => Friendly a -> Adj a
friToAdj = graphToAdj . friToGraph


-- 81

findPaths :: (Eq a) => [a] -> [(a,a)] -> [[a]]
findPaths [] _ = []
findPaths (xs) abs = map (\(a,b)-> xs ++ [b]) abx1
  where abx1 = filter (\(a,b) -> a == last xs && not (elem b xs)) abs

appendNodes :: (Eq a) => [[a]] -> [(a,a)] -> [[a]]
appendNodes ps nodes = concatMap (\p -> findPaths p nodes) ps

allPaths :: (Eq a) => [[a]] -> [(a,a)] -> [[a]]
allPaths ps nodes = if morePaths == [] then ps else (ps ++ (allPaths morePaths nodes))
  where morePaths = appendNodes ps nodes

paths :: (Eq a) => a -> a -> [(a,a)] -> [[a]]
paths a b nodes = filter (\p -> b == last p) $ allPaths ps1 nodes
  where ps1 = [[a]]

t81t1 = paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
t81t2 = paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

-- 82

findPaths82 :: (Eq a) => [a] -> [(a,a)] -> [[a]]
findPaths82 [] _ = []
findPaths82 (xs) abs = map (\(a,b)-> xs ++ [b]) abx1
  where abx1 = filter (\(a,b) -> a == last xs && not (elem b (tail xs))) abs

appendNodes82 :: (Eq a) => [[a]] -> [(a,a)] -> [[a]]
appendNodes82 ps nodes = concatMap (\p -> findPaths82 p nodes) ps

allPaths82 :: (Eq a) => [[a]] -> [(a,a)] -> [[a]]
allPaths82 ps nodes = if morePaths == [] then ps else (ps ++ (allPaths82 morePaths nodes))
  where morePaths = appendNodes82 ps nodes

cycle82 :: (Eq a) => a -> [(a,a)] -> [[a]]
cycle82 a nodes = filter ((/= 1) . length) $ filter (\p -> a == last p) $ allPaths82 ps1 nodes
  where ps1 = [[a]]

t82t1 = cycle82 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
t82t2 = cycle82 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

-- 83
k4 = Graph ['a', 'b', 'c', 'd']
     [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

paths' :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths' a b xs | a == b = [[a]]
              | otherwise = concat [map (a :) $ paths' d b $ [x | x <- xs, x /= (c, d)]
                                   | (c, d) <- xs, c == a] ++ 
                            concat [map (a :) $ paths' c b $ [x | x <- xs, x /= (c, d)]
                                   | (c, d) <- xs, d == a]
 
cycle' :: (Eq a) => a -> [(a, a)] -> [[a]]
cycle' a xs = [a : path | e <- xs, fst e == a, path <- paths' (snd e) a [x | x <- xs, x /= e]] ++
              [a : path | e <- xs, snd e == a, path <- paths' (fst e) a [x | x <- xs, x /= e]]
 

acc e es = es ++ (map (e:) es)
alltrees ys = [edges | edges <- foldr acc [[]] ys]
t83t10 = alltrees [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

spantree :: (Eq a) => Graph a -> [Graph a]
spantree (Graph xs ys) = filter (connected) $ filter (not . cycles) $ filter (nodes) alltrees
   where
      alltrees = [Graph (ns edges) edges | edges <- foldr acc [[]] ys]
      acc e es = es ++ (map (e:) es)
      ns es = foldr (\x xs -> if x `elem` xs then xs else x:xs) 
             [] $ concat $ map (\(a, b) -> [a, b]) es
      nodes (Graph xs' ys') = length xs == length xs'
      cycles (Graph xs' ys') = any ((/=) 0 . length . flip cycle' ys') xs'
      connected (Graph (x':xs') ys') = not $ any (null) [paths' x' y' ys' | y' <- xs']

t83t1 = spantree k4

-- 85
graphG1 = Graph [1, 2, 3, 4, 5, 6, 7, 8]
      [(1, 5), (1, 6), (1, 7), (2, 5), (2, 6), (2, 8),
       (3, 5), (3, 7), (3, 8), (4, 6), (4, 7), (4, 8)]
 
graphH1 = Graph [1, 2, 3, 4, 5, 6, 7, 8]
      [(1, 2), (1, 4), (1, 5), (6, 2), (6, 5), (6, 7),
       (8, 4), (8, 5), (8, 7), (3, 2), (3, 4), (3, 7)]
 

theSame :: a -> b -> Bool
theSame _ _ = True

isomorphic :: (Eq a, Eq b, Enum a, Enum b, Ord a, Ord b) => Graph a -> Graph b -> Bool
isomorphic g1@(Graph ns1 es1) g2@(Graph ns2 es2) = length ns1 == length ns2 && length es1 == length es2 && theSame g1 g2 && canon g1 == canon g2

canon :: (Ord a, Enum a) => Graph a -> String
canon g = minimum $ map f $ perm $ length a
   where
      perm n = foldr (\x xs -> [i : s | i <- [1..n], s <- xs, i `notElem` s]) [[]] [1..n]
      find a x = let (xs, ys) = break ((==) (fst x) . fst) a in head ys
      Adj a = graphToAdj g
      v = map fst a
      -- [(node, [nodes])] -> [(Int, [Int])]
      f p = let n = zip v p -- [ (node, Int) ]
            in show [(snd x,  -- [(Int, [Int])]
          sort id $ map (\x -> 
             snd $ head $ snd $ break ((==) x . fst) n) $ snd $ find a x)
        | x <- sort snd n]
      sort f n = foldr (\x xs -> let (lt, gt) = break ((<) (f x) . f) xs
         in lt ++ [x] ++ gt) [] n

-- 86
degree :: (Eq a) => a -> Graph a -> Int
degree a g = let Adj adj = graphToAdj g
             in length . snd . head $ filter (\(n,ns) -> n == a) adj


sortByDegree :: (Eq a) => Graph a -> [(a, Int)]
sortByDegree g@(Graph ns es) = sortBy f $ map (\n -> (n,degree n g)) ns
  where f (d1, n1) (d2, n2) = if n1 > n2 then GT else LT

petersen = Graph ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
     [('a', 'b'), ('a', 'e'), ('a', 'f'), ('b', 'c'), ('b', 'g'), 
      ('c', 'd'), ('c', 'h'), ('d', 'e'), ('d', 'i'), ('e', 'j'), 
      ('f', 'h'), ('f', 'i'), ('g', 'i'), ('g', 'j'), ('h', 'j')]

-- use diff Int value as diff color
kcolor :: (Eq a) => Graph a -> [(a, Int)]
kcolor g@(Graph (n:ns) es) = fst $ helper [] adj
  where Adj adj = graphToAdj g
        helper :: (Eq a) => [(a, Int)] -> [(a, [a])] -> ([(a,Int)], [(a,[a])])
        helper colored [] = (colored, [])
        helper colored adj = let (n1, ns1) = head adj
                                 coloredNodes = map fst colored
                                 n2c n = snd . head $ filter (\(n1,c1) -> n1 == n) colored
                                 cannotUseColors = map n2c $ filter (\n -> n `elem` coloredNodes) ns1
                                 validColor = head $ filter (\c -> not (c `elem` cannotUseColors)) [1..]
                             in helper ((n1,validColor):colored) (tail adj)


t86t1 = degree 'a' petersen
t86t2 = sortByDegree petersen

showColored = map n2cNode adj
 where
  Adj adj = graphToAdj petersen
  colers = kcolor petersen
  n2c n = snd . head $ filter (\(n1,c1) -> n1 == n) colers
  n2cNode (n1,ns1) = ((n2c n1), (map n2c ns1))


-- 87

findNextNode :: (Eq a) => a -> (a,a) -> a
findNextNode a (l,r) = if l == a then r else l

split :: (Eq a) => a -> [a] -> [(a, a)] -> ([(a,a)], [(a,a)])
split n1 exist es1 = let les = takeWhile (\(l,r) -> l /= n1 && r /= n1 && (l `notElem` exist) && (r `notElem` exist)) es1
                         res = dropWhile (\(l,r) -> l /= n1 && r /= n1 && (l `notElem` exist) && (r `notElem` exist)) es1
               in (les, res)
depthfirst :: (Eq a) => Graph a -> a -> [a]
depthfirst g@(Graph ns es) a = nodes
  where 
        (nodes, _, _) = next [a] [a] es
        next :: (Eq a) => [a] -> [a] -> [(a,a)] -> ([a], [a], [(a,a)])
        next result s [] = (result, s, [])
        next result [] _ = (result, [], [])
        next result s es1 = let n1 = last s
                                (les, res) = split n1 (init s) es1
                            in (if (null res)
                                then (next result (init s) es1)
                                else let nextNode = (findNextNode n1 $ head res)
                                     in next (if nextNode `elem` result then result else result ++ [nextNode]) (s ++ [nextNode]) (les ++(tail res)))

t87t1 = depthfirst (Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)]) 1

-- 88
connectedcomponents :: (Eq a) => Graph a -> [[a]]
connectedcomponents (Graph [] _) = []
connectedcomponents (Graph (top:v) e)
    | remaining == [] = [connected]
    | otherwise = connected : connectedcomponents (Graph remaining e)
    where
        connected = depthfirst (Graph (top:v) e) top
        remaining = (top:v) \\ connected

t88t1 = connectedcomponents (Graph [1,2,3,4,5] [(2,3),(3,4),(1,5)])
