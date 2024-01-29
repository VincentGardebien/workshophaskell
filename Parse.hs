filterArr :: (a -> Bool) -> [a] -> [a]
filterArr _ [] = []
filterArr f (x:xs) | f x = x : filterArr f xs
                   | otherwise = filterArr f xs