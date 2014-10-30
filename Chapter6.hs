module Chapter6 where

import           Prelude hiding ((++))

{-

WAVE!!!!!

Recursions

a function that calls itself
because of immutability (no state mutation), recursion is the only way to 'loop'

-}

sum' :: Num a => [a] -> a
sum' []       = 0
sum' (x : xs) = x + sum' xs

product' :: Num a => [a] -> a
product' []       = 1
product' (x : xs) = x * product xs

factorial :: Int -> Int
factorial n = product' [1..n]

{-

Recursion on Lists

-}

length' :: [a] -> Int
length' []       = 0
length' (_ : xs) = 1 + length' xs

(++) :: [a] -> [a] -> [a]
[] ++ ys       = ys
(x : xs) ++ ys = x : (xs ++ ys)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x []                   = [x]
insert x (y : ys) | x <= y    = x : y : ys
                  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []       = []
isort (x : xs) = insert x (isort xs)

{-

Multiple Arguments

-}

zip' :: [a] -> [b] -> [(a, b)]
zip' (x : xs) (y: ys) = (x, y) : zip xs ys
zip' _ _              = []

take' :: Int -> [a] -> [a]
take' _ []                   = []
take' n (x : xs) | n <= 0    = []
                 | otherwise = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ []                        = []
drop' n list@(_ : xs) | n <= 0    = list
                      | otherwise = drop' (n - 1) xs

{-

Multiple Recursion

-}


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

qsort :: Ord a => [a] -> [a]
qsort []       = []
qsort (x : xs) = qsort left ++ [x] ++ qsort right
                 where left  = [l | l <- xs, l <= x]
                       right = [r | r <- xs, r > x]

{-

Mutual Recursion
2 or more functions that call each other recursively
-}

-- taking even postions on the list
evens :: [a] -> [a]
evens []       = []
evens (x : xs) = x : odds xs


-- taking odd positions on the list
odds :: [a] -> [a]
odds []       = []
odds (_ : xs) = evens xs

{-

Advice on reursions

1. Define the type
2. Enumerate the cases
3. Define the simple case
4. Define other cases
5. Generalize and Simplify

-}
