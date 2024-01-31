module Parse where
import Data.Char (isDigit)

isNum :: String -> Bool
isNum [] = False
isNum (x:xs) | isDigit x = isNum xs
             | otherwise = False

isOperator :: String -> Bool
isOperator [] = False
isOperator (x:xs) | x == '+' || x == '-' || x == '*' || x == '/' = True
                  | otherwise = False


filterArr :: (a -> Bool) -> [a] -> [a]
filterArr _ [] = []
filterArr f (x:xs) | f x = x : filterArr f xs
                   | otherwise = filterArr f xs

parseLine :: [String] -> [String]
parseLine [] = []
parseLine (x:xs) | isNum x = x : parseLine xs
                 | isOperator x = x : parseLine xs
                 | otherwise = parseLine xs
