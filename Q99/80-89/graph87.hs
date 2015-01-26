type Node = Int
type Edge = (Node,Node)
type Graph = ([Node],[Edge])
 
depthfirst :: Graph -> Node -> [Node]
depthfirst (v,e) n
    | [x|x<-v,x==n] == [] = []
    | otherwise = dfrecursive (v,e) [n]
 
dfrecursive :: Graph -> [Node] -> [Node]
dfrecursive ([],_) _ = []
dfrecursive (_,_) [] = []
dfrecursive (v,e) (top:stack)
    | [x|x<-v,x==top] == [] = dfrecursive (newv, e) stack
    | otherwise = top : dfrecursive (newv, e) (adjacent ++ stack)
    where
        adjacent = [x | (x,y)<-e,y==top] ++ [x | (y,x)<-e,y==top]
        newv = [x|x<-v,x/=top]
