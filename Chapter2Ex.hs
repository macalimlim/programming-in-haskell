{-# LANGUAGE NoImplicitPrelude #-}

module Chapter2Ex where

import           Prelude (div, error, otherwise, undefined, (+), (++), (-),
                          (==), (>))

-- 1. Parenthesise the following arithmetic expressions:

{-

2 ^ 3 * 4
(2 ^ 3) * 4

2 * 3 + 4 * 5
(2 ∗ 3) + (4 ∗ 5)

2 + 3 * 4 ^ 5
2 + (3 * (4 ^ 5))

-}

-- 2. Work through the examples from this chapter using ghci.

{-

-}

-- 3. The script below contains three syntactic errors. Correct these errors and then check that your script works properly using ghci.

{-

N = a ’div’ length xs
    where
      a = 10
     xs = [1, 2, 3, 4, 5]

-}

n = a `div` length xs
    where a = 10
          xs = [1, 2, 3, 4, 5]

-- 4. Show how the library function last that selects the last element of a non-empty list could be defined in terms of the library functions introduced in this chapter. Can you think of another possible definition?

length []       = 0
length (x : xs) = 1 + length xs

[] !! n                   = error "invalid index"
(x : xs) !! 0             = x
(x : xs) !! n | n > 0     = xs !! (n - 1)
              | otherwise = error "invalid index"

last' xs = xs !! (length xs - 1)

{-

last' [1, 2, 3, 4, 5]
[1, 2, 3, 4, 5] !! (length [1, 2, 3, 4, 5] - 1)
[1, 2, 3, 4, 5] !! ((1 + length [2, 3, 4, 5]) - 1)
[1, 2, 3, 4, 5] !! ((1 + (1 + length [3, 4, 5])) - 1)
[1, 2, 3, 4, 5] !! ((1 + (1 + (1 + length [4, 5]))) - 1)
[1, 2, 3, 4, 5] !! ((1 + (1 + (1 + (1 + length [5])))) - 1)
[1, 2, 3, 4, 5] !! ((1 + (1 + (1 + (1 + (1 + length []))))) - 1)
[1, 2, 3, 4, 5] !! ((1 + (1 + (1 + (1 + (1 + (0)))))) - 1)
[2, 3, 4, 5] !! (((1 + (1 + (1 + (1 + (1 + (0)))))) - 1) - 1)
[3, 4, 5] !! ((((1 + (1 + (1 + (1 + (1 + (0)))))) - 1) - 1) - 1)
[4, 5] !! (((((1 + (1 + (1 + (1 + (1 + (0)))))) - 1) - 1) - 1) - 1)
[5] !! ((((((1 + (1 + (1 + (1 + (1 + (0)))))) - 1) - 1) - 1) - 1) - 1)
[5] !! 0
5

-}

head []       = error "empty list"
head (x : xs) = x

reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

last'' xs = head (reverse xs)

{-

last'' [1, 2, 3, 4, 5]
head (reverse [1, 2, 3, 4, 5])
head (reverse [2, 3, 4, 5] ++ [1])
head ((reverse [3, 4, 5] ++ [2]) ++ [1])
head (((reverse [4, 5] ++ [3]) ++ [2]) ++ [1])
head ((((reverse [5] ++ [4]) ++ [3]) ++ [2]) ++ [1])
head (((((reverse [] ++ [5]) ++ [4]) ++ [3]) ++ [2]) ++ [1])
head (((((([]) ++ [5]) ++ [4]) ++ [3]) ++ [2]) ++ [1])
head [5, 4, 3, 2, 1]
5

-}

last''' []       = error "empty list"
last''' [x]      = x
last''' (x : xs) = last''' xs

{-

last''' [1, 2, 3, 4, 5]
last''' [2, 3, 4, 5]
last''' [3, 4, 5]
last''' [4, 5]
last''' [5]
5

-}

-- 5. Show how the library function init that removes the last element from a non-empty list could similarly be defined in two different ways.

take n []       = []
take 0 xs       = xs
take 1 (x : xs) = [x]
take n (x : xs) = x : take (n - 1) xs

init' xs = take (length xs - 1) xs

{-

init' [1, 2, 3, 4, 5]
take (length [1, 2, 3, 4, 5] - 1) [1, 2, 3, 4, 5]
take ((1 + length [2, 3, 4, 5]) - 1) [1, 2, 3, 4, 5]
take ((1 + (1 + length [3, 4, 5])) - 1) [1, 2, 3, 4, 5]
take ((1 + (1 + (1 + length [4, 5]))) - 1) [1, 2, 3, 4, 5]
take ((1 + (1 + (1 + (1 + length [5])))) - 1) [1, 2, 3, 4, 5]
take ((1 + (1 + (1 + (1 + (1 + length []))))) - 1) [1, 2, 3, 4, 5]
take ((1 + (1 + (1 + (1 + (1 + (0)))))) - 1) [1, 2, 3, 4, 5]
take 4 [1, 2, 3, 4, 5]
1 : (take 3 [2, 3, 4, 5])
1 : (2 : (take 2 [3, 4, 5]))
1 : (2 : (3 : (take 1 [4, 5])))
1 : (2 : (3 : ([4])))
[1, 2, 3, 4]

-}

drop n [] = []
drop 0 xs = xs
drop 1 (x : xs) = xs
drop n (x : xs) = drop (n - 1) xs

init'' xs = reverse (drop 1 (reverse xs))

{-

init'' [1, 2, 3, 4, 5]
reverse (drop 1 (reverse [1, 2, 3, 4, 5]))
reverse (drop 1 (reverse [2, 3, 4, 5] ++ [1]))
reverse (drop 1 ((reverse [3, 4, 5] ++ [2]) ++ [1]))
reverse (drop 1 (((reverse [4, 5] ++ [3]) ++ [2]) ++ [1]))
reverse (drop 1 ((((reverse [5] ++ [4]) ++ [3]) ++ [2]) ++ [1]))
reverse (drop 1 ((((([] ++ [5]) ++ [4]) ++ [3]) ++ [2]) ++ [1]))
reverse (drop 1 [5, 4, 3, 2, 1])
reverse [4, 3, 2, 1]
(reverse [3, 2, 1]) ++ [4]
((reverse [2, 1] ++ [3]) ++ [4])
(((reverse [1] ++ [2]) ++ [3]) ++ [4])
((((reverse [] ++ [1]) ++ [2]) ++ [3]) ++ [4])
((((([]) ++ [1]) ++ [2]) ++ [3]) ++ [4])
[1, 2, 3, 4]

-}

init''' []       = error "empty list"
init''' [x]      = []
init''' (x : xs) = x : init''' xs

{-

init''' [1, 2, 3, 4, 5]
1 : (init''' [2, 3, 4, 5])
1 : (2 : (init''' [3, 4, 5]))
1 : (2 : (3 : init''' [4, 5]))
1 : (2 : (3 : (4 : init''' [5])))
1 : (2 : (3 : (4 : [])))
[1, 2, 3, 4]

-}
