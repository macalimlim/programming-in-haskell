module CountdownProblem where

import           Data.List

{-

Countdown is a popular quiz programme that has been running on British
television since 1982, and includes a numbers game that we shall refer to as
the countdown problem. The essence of the problem is as follows:

Given a sequence of numbers and a target number, attempt to construct an
expression whose value is the target, by combining one or more numbers from the
sequence using addition, subtraction, multiplication, division.

Example

Using a list of numbers

[1, 3, 7, 10, 25, 50]

and four basic math operations

* / + -

construct an expression whose value is 765

Rules

All numbers, including intermediate results must be positive integers

Each number from the list can only be used once

-}

data Op = Add | Sub | Mul | Div
          deriving Show

data Expr = Val Int | App Op Expr Expr
            deriving Show

applyOp :: Op -> Int -> Int -> Int
applyOp Add x y = x + y
applyOp Sub x y = x - y
applyOp Mul x y = x * y
applyOp Div x y = x `div` y

isValid :: Op -> Int -> Int -> Bool
isValid Add _ _ = True
isValid Sub x y = x > y
isValid Mul _ _ = True
isValid Div x y = x `mod` y == 0

evalExpr :: Expr -> [Int]
evalExpr (Val n)       = [n | n > 0]
evalExpr (App o e1 e2) = [applyOp o x y | x <- evalExpr e1
                                        , y <- evalExpr e2
                                        , isValid o x y]

getChoices :: [Int] -> [[Int]]
getChoices ns = nub $ subsequences ns ++ permutations ns

getValues :: Expr -> [Int]
getValues (Val n)       = [n]
getValues (App _ e1 e2) = getValues e1 ++ getValues e2

isSolution :: Expr -> [Int] -> Int -> Bool
isSolution e ns t = getValues e `elem` getChoices ns && evalExpr e == [t]

split :: [Int] -> [([Int], [Int])]
split ns = [splitAt len ns | len <- [1.. (length ns - 1)]]

combineExprs :: Expr -> Expr -> [Expr]
combineExprs e1 e2 = [App o e1 e2 | o <- [Add, Sub, Mul, Div]]

getExprs :: [Int] -> [Expr]
getExprs []  = []
getExprs [n] = [Val n]
getExprs ns  = [e | (f, s) <- split ns
                  , l      <- getExprs f
                  , r      <- getExprs s
                  , e      <- combineExprs l r]

getSolutions :: [Int] -> Int -> [Expr]
getSolutions ns t = [e | ns' <- getChoices ns
                       , e   <- getExprs ns'
                       , evalExpr e == [t]]

--

type Result = (Expr, Int)

getResults :: [Int] -> [Result]
getResults ns = [(e, t) | e <- getExprs ns
                        , t <- evalExpr e]

combineResults :: Result -> Result -> [Result]
combineResults (e1, t1) (e2, t2) = [(App o e1 e2, applyOp o t1 t2)
                                     | o <- [Add, Sub, Mul, Div]
                                     , isValid o t1 t2]

getResults' :: [Int] -> [Result]
getResults' []  = []
getResults' [n] = [(Val n, n) | n > 0]
getResults' ns  = [res | (f, s) <- split ns
                       , r1     <- getResults' f
                       , r2     <- getResults' s
                       , res    <- combineResults r1 r2]

getSolutions' :: [Int] -> Int -> [Expr]
getSolutions' ns t = [e | ns'     <- getChoices ns
                        , (e, t') <- getResults' ns'
                        , t == t']

--

isValid' :: Op -> Int -> Int -> Bool
isValid' Add x y = x <= y
isValid' Sub x y = x > y
isValid' Mul x y = x <= y && x /= 1 && y /= 1
isValid' Div x y = x `mod` y == 0 && y /= 1

combineResults' :: Result -> Result -> [Result]
combineResults' (e1, t1) (e2, t2) = [(App o e1 e2, applyOp o t1 t2)
                                      | o <- [Add, Sub, Mul, Div]
                                      , isValid' o t1 t2]

getResults'' :: [Int] -> [Result]
getResults'' []  = []
getResults'' [n] = [(Val n, n) | n > 0]
getResults'' ns  = [res | (f, s) <- split ns
                        , r1     <- getResults'' f
                        , r2     <- getResults'' s
                        , res    <- combineResults' r1 r2]

getSolutions'' :: [Int] -> Int -> [Expr]
getSolutions'' ns t = [e | ns'     <- getChoices ns
                         , (e, t') <- getResults'' ns'
                         , t == t']
