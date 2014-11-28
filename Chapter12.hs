module Chapter12 where

-- Lazy Evaluation

-- evaluate an expression when it is needed

inc :: Int -> Int
inc n = n + 1

{-

Eager Evaluation or CBV

inc (2 * 3)
inc 6
6 + 1
7

Lazy Evaluation or CBN

inc (2 * 3)
(2 * 3) + 1
6 + 1
7

-}

{-

This does not hold for most imperative languages

CBN
n = 0
n + (n = 1)
0 + (n = 1)
0 + 1
1

CBV
n = 0
n + (n = 1)
n + 1
1 + 1
2

-}

mult :: (Int, Int) -> Int
mult (x, y) = x * y

{-

CBV
mult (1 + 2, 2 + 3)
mult (3, 2 + 3)
mult (3, 5)
3 * 5
15

CBN
mult (1 + 2, 2 + 3)
(1 + 2) * (2 + 3)
3 * (2 + 3)
3 * 5
15

-}

-- Lambda Expressions

mult' :: Int -> Int -> Int
mult' x = \ y -> x * y

{-

CBV
mult (1 + 2) (2 + 3)
mult 3 (2 + 3)
(\ y -> 3 * y) (2 + 3)
(\ y -> 3 * y) 5
3 * 5
15

CBN
mult (1 + 2) (2 + 3)
(\ y -> (1 + 2) * y) (2 + 3)
(1 + 2) * (2 + 3)
3 * (2 + 3)
3 * 5
15

-}

-- Termination

inf :: Int
inf = 1 + inf

{-

Both CBV and CBN will not terminate

inf
1 + inf
1 + 1 + inf
1 + 1 + 1 + inf
1 + 1 + 1 + 1 + ......

-}

fst' :: (Int, Int) -> Int
fst' (x, _) = x

{-
CBV
fst (0, inf)
fst (0, 1 + inf)
fst (0, 1 + 1 + .....)

CBN
fst (0, inf)
0

-}

-- Number of reductions

square :: Int -> Int
square x = x * x

{-

CBV
square (1 + 2)
square 3
3 * 3
9

CBN
square (1 + 2)
(1 + 2) * (1 + 2)
3 * (1 + 2)
3 * 3
9

-}

-- Infinite Structures

ones :: [Int]
ones = 1 : ones

{-

Both CBV and CBN

ones
1 : ones
1 : 1 : ones
1 : 1 : 1 : ......

CBV
head ones
head (1 : ones)
head (1 : 1 : ones)
head (1 : 1 : 1 : .......)

CBN
head ones
head (1 : ones)
1

take 3 ones
[1, 1, 1]

filter (<= 5) [1..] will not terminate

takeWhile (<= 5) [1..] will terminate

-}

-- Strict Application (using eager evaluation)

{-

use the $! function

square $! (1 + 2)
square $! 3
square 3
3 * 3
9

-}
