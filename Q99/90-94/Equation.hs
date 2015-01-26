module P93 where

import Control.Monad
import Data.List
import Data.Maybe

data Expr = Const Integer | Binary Expr Op Expr
  deriving (Eq)
data Op = Plus | Minus | Multiply | Divide
  deriving (Enum, Show, Eq, Bounded)

type Equation = (Expr, Expr)
type Value = Rational

splitArray :: [a] -> [([a], [a])]
splitArray xs = [ splitAt n xs | n <- [1..(length xs - 1)]]


allOp = [Plus .. Divide]


maybe2list :: Maybe a -> [a]
maybe2list Nothing = []
maybe2list (Just a) = [a]

apply :: Op -> Value -> Value -> Maybe Value
apply Plus x y = Just (x + y)
apply Minus x y = Just (x - y)
apply Multiply x y = Just (x * y)
apply Divide x 0 = Nothing
apply Divide x y = Just (x / y)


-- e1 op (e2 op' e3) == (e1 op e2) op' e3
rightAssociative :: Op -> Expr -> Bool
rightAssociative Plus (Binary _ Plus _) = True
rightAssociative Plus (Binary _ Minus _) = True
rightAssociative Multiply (Binary _ Multiply _) = True
rightAssociative Multiply (Binary _ Divide _) = True
rightAssociative _ _ = False

exprs :: [Integer] -> [(Expr, Value)]
exprs [n] = [(Const n, fromInteger n)]
exprs ns = [ (Binary e1 op e2, v) | (ns1, ns2) <- splitArray ns,
                                    (e1, v1) <- exprs ns1,
                                    (e2, v2) <- exprs ns2,
                                    op <- allOp,
                                    not (rightAssociative op e2),
                                    v <- maybe2list (apply op v1 v2)
           ]


--instance Show Op where
--  show Plus = "+"
--  show Minus = "-"
--  show Multiply = "*"
--  show Divide = "/"

instance Show Expr where
  show (Const a) = show a
  show (Binary a op b) = show a ++ " " ++ show op ++ " " ++ show b


findEquations :: [Integer] -> [Equation]
findEquations xs = [(e1,e2) | (xs1, xs2) <- splitArray xs,
                              (e1, v1) <- exprs xs1, (e2, v2) <- exprs xs2,
                              v1 == v2
                   ]

showsEquation :: Equation -> ShowS
showsEquation (l, r) = showsExprPrec 0 l . showString " = " . showsExprPrec 0 r
 
-- all operations are left associative
showsExprPrec :: Int -> Expr -> ShowS
showsExprPrec _ (Const n) = shows n
showsExprPrec p (Binary e1 op e2) = showParen (p > op_prec) $
        showsExprPrec op_prec e1 . showString (opName op) .
                showsExprPrec (op_prec+1) e2
  where op_prec = precedence op
 
precedence :: Op -> Int
precedence Plus = 6
precedence Minus = 6
precedence Multiply = 7
precedence Divide = 7

opName :: Op -> String
opName Plus = "+"
opName Minus = "-"
opName Multiply = "*"
opName Divide = "/"

test = mapM_ putStrLn $ map (flip showsEquation "") $ findEquations [2,3,5,7,11]
