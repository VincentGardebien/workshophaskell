# Workshop Programation Fonctionnelle

## Project aim

Do you remind about DoOp ? Our favorite calculator which calculate any arithmetic expression like this :
```
42 + 404 = 426
```
However, we cannot do a calculation like this
```
42 * 69 + 12 - 88 / 404
```
Our computer is not very smart. It doesn't know about arithmetic priority (like doing the multiplication or division before an addtion or a substraction). How can we do that ?
We will use an **abstract syntax tree**. It is a binary tree which allow us to do the right calculations in the right moment. The objective is now set.

## Requirements
- This project will be entirely made the programming language Haskell.
- Make sure you have `ghc` with `ghc --version`. Otherwise, [download it here](https://www.haskell.org/ghcup/).
- Create a repository in **public** and send me the link in the Teams group.
- We will not talk about Monads.

## Useful links
- [Abstract Syntax Tree (please not Java)](https://www.geeksforgeeks.org/abstract-syntax-tree-ast-in-java/)
- [Tree traversal : which one ?](https://www.geeksforgeeks.org/tree-traversals-inorder-preorder-and-postorder/)

## GHCi - Our beautiful sandbox

### Step 0 - Hello World

- Use `ghci` to print on your terminal to print `Hello World`.
```
❯ ghci
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
ghci> "Hello World!"
"Hello World!"
```
Like in Vim, use `:q` to quit `ghci`.

### Step 1 - create a sum function
- In Haskell, we already have a sum function, but it is the easiest function to begin with Haskell.
```
function : mySum
prototype : mySum :: [Int] -> Int
file : MySum.hs
```
- To test it, use `ghci` and load the module.
```
❯ ghci
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
ghci> :l MySum.hs
[1 of 2] Compiling Main             ( MySum.hs, interpreted )
Ok, one module loaded.
ghci> mySum [10, 5, 5]
20
```
You may use `:r` to reload the file once the file is loaded.

### Step 2.1 - Do the basics operation functions
- Now you have some basics in Haskell, we will create all our operations functions.
```
functions : addition, substract, multiply
file : Operation.hs

-- adition
prototype : addition :: Int -> Int -> Int

-- substraction
prototype : substraction :: Int -> Int -> Int

-- multiply
prototype : multiply :: Int -> Int -> Int
```
You now know how to test your functions (hehe).

### Step 2.2 - Divide...?
```
function : divide
prototype : divide :: Int -> Int -> ??? Int
file : Operation.hs
```
Should I give you the answer about the prototype ? `Maybe` yes, `Maybe` no...

### Step 3 - Our first program !
You have some little functions. It is enough to create a program, and let you imagine how we will retrieve datas with a program. In real life, a calculator doesn't work with `ghci`, isn't it ?
- Your `Operations.hs` file should now be a module.
```hs
module Operations where
-- Your operation functions
```
- Create a `Main.hs` file. We will now do some imperative programming, but please, avoid this type of programming when you code in functionnal programming.
```
function : main
prototype : main::IO ()
file : Main.hs
```
The `IO()` class is absolutely necessary. You should end your functions by a `IO` instruction, such as `print`. You should also import `System.Environment` and `System.Exit`. Also, you have a module you created yourself. Why not implementing it ?
```hs
-- read the paragraph above
main :: IO()
main = do
    -- do your business
    print "Hello world!" -- "print" is an IO function. You should always finish by an IO. 
```
Also, you have a Makefile to compile your program. Modify it as you want.
### Step 4 - DoOp
You have all necessary tools to make a `DoOp` program. Just a few thing I'd like to add : your program should retrieve line by line your basic operation, like `1 + 2`.
For the divide operation, make sure you handle it correctly.

# Advanced steps

Congratulations ! You achieved your first Haskell program successfully ! \
Now you're here, we're going to build our tree. Since the main core of your program is set up, only few more implementations is required. However, you had better take a few time to understand what we are going to do. Maybe it could give you ideas on your C (tek1) / C++ (tek2) projects ? \
One more thing for _tek2_ students : you could follow my steps below, or you could do the assignment _in your own way_. 

## Callback functions, sort and cool things
### Step 5 - Predicates
- A predicate is a (like in maths) function that takes parameters and return a boolean.
- Create a function that check if the string is a operator.
- Create a function that check if the string is a number. \
For this function, don't be sophisticated. Use `isDigit`.

### Step 6 - filterArr
- Create a function that filters only if the predicate is valid.
- The filter function **MUST** take a predicate as a parameter (callback function).
```
function : filterArr
prototype : filterArr :: (a -> Bool) -> [a] -> [a]
file : Parse.hs
```
```
ghci> :l Parse.hs
[1 of 1] Compiling Parse            ( Parse.hs, interpreted )
Ok, one module loaded.
ghci> filterArr isNum ["1", "2", "+", "42"]
["1","2","42"]
ghci> filterArr isOperator ["1", "2", "+", "42"]
["+"]
ghci> filterArr (\ x -> x `mod` 2 == 0) [1, 2, 3, 4, 5]
[2,4]
```

### Step 7 - parseLine
```
function : parseLine
prototype : parseLine :: [a] -> [a]
file : Parse.hs
```
```
ghci> parseLine ["1", "+", "2", "-", "15", "/", "42"]
["+","-","/","1","2","15","42"]
```
- Perhaps previous functions are useful for this one...

## Abstract Syntax Tree
In the following steps, you are going to manipulate the `Tree` data structure defined in the `Tree.hs` file. We are going to use this file. \
I assume you have read links above.

### ["Treetorial"](https://youtu.be/2Fka4_Cf2M4?si=ooFQIj7qiIu61jwL)
Welcome everyone ! Little tutorial to handle the AST data structure ! \
In your ghci, for testing your functions, I've written a little guide above ! 
- Show the value
```
ghci> Value 42
42
```
- Show an addition
```
ghci> Add (Value 21) (Value 21)
21 + 21
```
- Show a substraction
```
ghci> Sub (Value 84) (Value 84)
84 - 84
```
- Show a multiplication followed by a division
```
ghci> Mul (Value 2) (Div (Value 84) (Value 2))
2 * 84 / 2
```
Don't forget like, share and suscribe to the channel <3

### Step 8 - Build an expression
Jokes aside, we are going to build the ASTree.
- Create a function that takes two ASTree values and the string operand.
- This function should return the right ASTree expression depending on the operation given.
```
function : buildExpr
prototype : buildExpr :: ASTree -> String -> ASTree -> ASTree
file : Tree.hs
```
```
ghci> buildExpr (Value 3) "+" (Value 10)
3 + 10
```
### Step 9 - mapValues
```
function : mapValues
prototype : mapValues :: [String] -> [ASTree]
file : Tree.hs
```
- Use the `map` function to recursively convert each element of the array to Value.
- `map` works like `filterArr`, it take a function, and apply it to all the array.
- Therefore build the right callback function.
```
function : mapValues
prototype : mapValues :: [String] -> [ASTree]
file : Tree.hs
```
```
ghci> mapValues ["1", "2", "3"]
[1,2,3]
```

### Step 10 - Expression list handling
```
function : buildExprList
prototype : buildExprList :: [String] -> [ASTree] -> ASTree
file : Tree.hs
```
- This one is a little trickier.
- This function build each expressions from both list.
- Each new built expressions return back to the ASTree list.
- Once the operation list is empty, return the final ASTree built.
- Please don't hesitate to ask for this one.

### ASTree building 
- With the previous function, build the ASTree.
```
function : buildAST
prototype : buildAST ([String, String] -> ASTree)
file : Tree.hs
```
```
```
## Final expression program
Wow ! You are here ? I'm really chokbar to be honest. \
To do the final calculation, you need to do a tree traversal, and do the right operation in the right time.

## Going further
- Add the true priority order (multiplication and division).
- Add parenthesis.
- Redo all little functions in Javascript ? Next workshop is Node.js.
Have fun !
