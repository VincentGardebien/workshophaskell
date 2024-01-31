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

manageAST :: ASTree -> Int
manageAST (Value a) = a
manageAST (Add a b) = manageAST a + manageAST b
manageAST (Sub a b) = manageAST a - manageAST b
manageAST (Mul a b) = manageAST a * manageAST b
manageAST (Div a b) = manageAST a `div` manageAST b
