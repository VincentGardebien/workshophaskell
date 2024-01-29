module Operation where

addition :: Int -> Int -> Int
addition a b = (a + b);

substraction :: Int -> Int -> Int
substraction a b = (a - b);

multiply :: Int -> Int -> Int
multiply a b = (a * b);

divide :: Int -> Int -> Maybe Int
divide a 0 = Nothing;
divide a b = Just(a `div` b);