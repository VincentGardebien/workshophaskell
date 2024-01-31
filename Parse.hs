import Data.Char (isDigit)

isNum :: String -> Bool
isNum [] = False
isNum = all isDigit

isOperator :: String -> Bool
isOperator [] = False
isOperator = (`elem` ["+", "-", "*", "/"])


filterArr :: (a -> Bool) -> [a] -> [a]
filterArr _ [] = []
filterArr f (x:xs) | f x = x : filterArr f xs
                   | otherwise = filterArr f xs

parseLine :: [String] -> [String]
parseLine [] = []
parseLine xs = filter (not . isNum) xs ++ filter isNum xs
