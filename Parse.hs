import Data.Char (isDigit)

filterArr :: (a -> Bool) -> [a] -> [a]
filterArr _ [] = []
filterArr f (x:xs) | f x = x : filterArr f xs
                   | otherwise = filterArr f xs

parseLine :: [String] -> [String]
parseLine xs = filter (not . isNum) xs ++ filter isNum xs
  where
    isNum = all isDigit