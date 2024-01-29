import  Operation
import  System.Environment
import System.Exit


doOperation :: String -> String -> String -> IO()
doOperation a b c 
    | c == "+" = putStrLn (show (addition (read a) (read b)))
    | c == "-" = putStrLn (show (substraction (read a) (read b)))
    | c == "*" = putStrLn (show (multiply (read a) (read b)))
    | c == "/" = putStrLn (show (divide (read a) (read b)))

doOp::[String] -> IO()
doOp args
    | length args /= 3 = exitWith (ExitFailure 84)
    | (args !! 1) /= "+" && (args !! 1) /= "-" && (args !! 1) /= "*" && (args !! 1) /= "/" = exitWith (ExitFailure 84)
    | otherwise = doOperation (args !! 0) (args !! 2) (args !! 1)

main::IO()
main = do

input <- getLine
let args = words input
doOp args
