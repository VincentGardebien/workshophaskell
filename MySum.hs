mySum:: [Int] -> Int
mySum [] = 0;
mySum (a:b) = a + mySum(b);