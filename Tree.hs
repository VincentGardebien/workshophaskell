module Tree where
data ASTree = Add ASTree ASTree
            | Sub ASTree ASTree
            | Mul ASTree ASTree
            | Div ASTree ASTree
            | Value Int
instance Show ASTree where
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2
    show (Value n)   = show n

buildExpr :: ASTree -> String -> ASTree -> ASTree
buildExpr e1 op e2 = case op of
    "+" -> Add e1 e2
    "-" -> Sub e1 e2
    "*" -> Mul e1 e2
    "/" -> Div e1 e2

mapValues :: [String] -> [ASTree]
mapValues [] = []
mapValues (x:xs) = Value (read x :: Int) : mapValues xs

buildExprList :: [String] -> [ASTree] -> ASTree
buildExprList [] [x] = x
buildExprList (op:ops) (x:y:xs) = buildExprList ops ((buildExpr x op y):xs)

buildAST :: ([String], [String]) -> ASTree
buildAST (ops, vals) = buildExprList ops (mapValues vals)