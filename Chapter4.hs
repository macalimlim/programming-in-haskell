module Chapter4 where

import           Prelude hiding ((&&))

{-

WAVE!!!! :D

Defining Functions

-}

isDigit' :: Char -> Bool
isDigit' c = (c >= '0') && (c <= '9')

even' :: Integral a => a -> Bool
even' x = x `mod` 2 == 0

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

{-

Conditional Expressions

-}

abs' :: Int -> Int
abs' x = if x >= 0 then x else (-x)


signum' :: Int -> Int
signum' n = if n < 0
               then (-1)
               else if n == 0
                       then 0
                       else 1
{-

Guarded Equations

-}

abs'' :: Int -> Int
abs'' n | n == 0    = 0
        | n > 0     = n
        | otherwise = (-n)

signum'' :: Int -> Int
signum'' n | n < 0  = (-1)
           | n == 0 = 0
           | n > 0  = 1

{-

Pattern Matching

-}

not' :: Bool -> Bool
not' True  = False
not' False = True

(&&) :: Bool -> Bool -> Bool
True && True   = True
True && False  = False
False && True  = False
False && False = False

--or

ands :: Bool -> Bool -> Bool
True `ands` True = True
_ `ands` _       = False

--or

ands2 :: Bool -> Bool -> Bool
True `ands2` x  = x
False `ands2` _ = False

{-

Tuple Patterns

-}

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, x) = x

{-

List Patterns

-}

test :: [Char] -> Bool
test ['a', _, _] = True
test _           = False

{-

matching on cons

[1, 2, 3]
1 : [2, 3]
1 : 2 : [3]
1 : 2 : 3 : []

-}

test' :: [Char] -> Bool
test' ('a' : _) = True
test' _         = False

null' :: [a] -> Bool
null' []      = True
null' (_ : _) = False

head' :: [a] -> a
head' (x : _) = x
head' []      = error "empty"

tail' :: [a] -> [a]
tail' []       = error "empty"
tail' (_ : xs) = xs

{-

Lambda Expressions
Anonymous Functions
"functions with no name"

-}

double :: Int -> Int
double = \ x -> x + x

{-

var double = function(x) {
    return x + x;
};

-}


add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> Int -> Int
add' y = \ x -> x + y

add'' :: Int -> Int -> Int
add'' = \ x -> \ y -> x + y

{-

Sections
same as currying but you could curry on both args on a infix op

-}
