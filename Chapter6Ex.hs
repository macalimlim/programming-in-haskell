module Chapter6Ex where

import           Prelude hiding (and, concat, drop, elem, init, last, length,
                          replicate, sum, take)

{-

1. Define the exponentiation operator ^ for non-negative integers using the
same pattern of recursion as the multiplication operator ∗, and show how
2 ↑ 3 is evaluated using your definition.

-}

(<^>) :: Int -> Int -> Int
n <^> 0                  = 1
n <^> 1                  = n
n <^> e | n > 1 && e > 1 = n * (n <^> (e - 1))
        | otherwise      = error "negative num"

{-

2. Using the definitions given in this chapter, show how length [1, 2, 3],
drop 3 [1, 2, 3, 4, 5], and init [1, 2, 3] are evaluated.

-}

length :: [a] -> Int
length []       = 0
length (_ : xs) = 1 + length xs

{-

length [1, 2, 3]
1 + length [2, 3]
1 + (1 + length [3])
1 + (1 + (1 + length []))
1 + (1 + (1 + 0))
1 + (1 + 1)
1 + 2
3
-}

drop :: Int -> [a] -> [a]
drop 0 xs                   = xs
drop _ []                   = []
drop n (x : xs) | n > 0     = drop (n - 1) xs
                | otherwise = []

{-

drop 3 [1, 2, 3, 4, 5]
drop (3 - 1) [2, 3, 4, 5]
drop 2 [2, 3, 4, 5]
drop (2 - 1) [3, 4, 5]
drop 1 [3, 4, 5]
drop (1 - 1) [4, 5]
drop 0 [4, 5]
[4, 5]

-}

init :: [a] -> [a]
init []       = error "empty list"
init [x]      = []
init (x : xs) = x : init xs

{-

init [1, 2, 3]
1 : init [2, 3]
1 : (2 : init [3])
1 : (2 : [])
1 : [2]
[1, 2]

-}

{-

3. Without looking at the definitions from the standard prelude, define the
following library functions using recursion.

– Decide if all logical values in a list are True :
and :: [Bool] -> Bool

-}

and :: [Bool] -> Bool
and []       = True
and (x : xs) = x && and xs

{-

and [True, False, True]
True && and [False, True]
True && (False && and [True])
True && (False && (True && and []))
True && (False && (True && True))
True && (False && True)
True && False
False

-}

{-

– Concatenate a list of lists:
concat :: [[a]] -> [a]

-}

concat :: [[a]] -> [a]
concat []       = []
concat (x : xs) = x ++ concat xs

{-

concat [[1, 2], [3, 4, 5], [6]]
[1, 2] ++ concat [[3, 4, 5], [6]]
[1, 2] ++ ([3, 4, 5] ++ concat [[6]])
[1, 2] ++ ([3, 4, 5] ++ ([6] ++ concat []))
[1, 2] ++ ([3, 4, 5] ++ ([6] ++ []))
[1, 2] ++ ([3, 4, 5] ++ [6])
[1, 2] ++ [3, 4, 5, 6]
[1, 2, 3, 4, 5, 6]

-}

{-

– Produce a list with n identical elements:
replicate :: Int -> a -> [a]

-}

replicate :: Int -> a -> [a]
replicate 0 _             = []
replicate 1 x             = [x]
replicate n x | n > 0     = x : replicate (n - 1) x
              | otherwise = []

{-

replicate 3 5
5 : replicate (3 - 1) 5
5 : replicate 2 5
5 : (5 : replicate (2 - 1) 5)
5 : (5 : replicate 1 5)
5 : (5 : [5])
5 : [5, 5]
[5, 5, 5]

-}

{-

– Select the nth element of a list:
(!!) :: [a] -> Int -> a

-}

nth :: [a] -> Int -> a
[] `nth` _                   = error "empty list"
(x : xs) `nth` n | n == 0    = x
                 | n > 0     = xs `nth` (n - 1)
                 | otherwise = error "negative num"

{-

[1, 2, 3, 4, 5] `nth` 3
[2, 3, 4, 5] `nth` (3 - 1)
[2, 3, 4, 5] `nth` 2
[3, 4, 5] `nth` (2 - 1)
[3, 4, 5] `nth` 1
[4, 5] `nth` (1 - 1)
[4, 5] `nth` 0
4

-}

{-

– Decide if a value is an element of a list:
elem :: Eq a => a -> [a] -> Bool

-}

elem :: Eq a => a -> [a] -> Bool
elem _ []                   = False
elem n (x : xs) | n == x    = True
                | otherwise = elem n xs

{-

elem 3 [1, 2, 3, 4, 5]
elem 3 [2, 3, 4, 5]
elem 3 [3, 4, 5]
True

-}

{-

Note: most of these functions are in fact defined in the prelude using other
library functions, rather than using explicit recursion.

-}

{-

4. Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that
merges two sorted lists to give a single sorted list. For example:
> merge [2, 5, 6] [1, 3, 4]
[1, 2, 3, 4, 5, 6]

Note: your definition should not use other functions on sorted lists such as
insert or isort , but should be defined using explicit recursion.

-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] []                                    = []
merge [] ys                                    = ys
merge xs []                                    = xs
merge list1@(x: xs) list2@(y : ys) | x <= y    = x : merge xs list2
                                   | otherwise = y : merge list1 ys

{-

merge [2, 5, 6] [1, 3, 4]
1 : merge [2, 5, 6] [3, 4]
1 : (2 : merge [5, 6] [3, 4])
1 : (2 : (3 : merge [5, 6] [4]))
1 : (2 : (3 : (4 : merge [5, 6] [])))
1 : (2 : (3 : (4 : [5, 6])))
1 : (2 : (3 : [4, 5, 6]))
1 : (2 : [3, 4, 5, 6])
1 : [2, 3, 4, 5, 6]
[1, 2, 3, 4, 5, 6]

-}

{-

5. Using merge , define a recursive function msort :: Ord a => [a] -> [a] that
implements merge sort, in which the empty list and singleton lists are already
sorted, and any other list is sorted by merging together the two lists that
result from sorting the two halves of the list separately.

Hint: first define a function halve :: [a] -> ([a], [a]) that splits a list into
two halves whose lengths differ by at most one.

-}

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = let (f, s) = halve xs
            in merge f s

{-

msort [2, 5, 6, 1, 3, 4]
merge [2, 5, 6] [1, 3, 4]
....same as above....

-}

{-

6. Using the five-step process, define the library functions that calculate the
sum of a list of numbers, take a given number of elements from the start of
a list, and select the last element of a non-empty list.

-}

{-

sum

1. define the type

sum :: [Int] -> Int

2. enumerate the cases

sum []
sum [x]
sum [x, y]
sum (x : xs)

3. define the simple cases

sum []       = 0
sum [x]      = x
sum [x, y]   = x + y
sum (x : xs) = ...

4. define other cases

sum []       = 0
sum [x]      = x
sum [x, y]   = x + y
sum (x : xs) = x + sum xs

5. generalize and simplify

sum []       = 0
sum (x : xs) = x + sum xs

-}

sum :: [Int] -> Int
sum []       = 0
sum (x : xs) = x + sum xs

{-

take

1. define the type

take :: Int -> [a] -> [a]

2. enumerate the cases

take n []
take 0 xs
take n (x : xs)

3. define simple cases

take n [] = []
take 0 xs = []
take n (x : xs) = ...

4. define other cases

take n [] = []
take 0 xs = []
take n (x : xs) | n > 0     = x : take (n - 1) xs
                | otherwise = []

5. generalize and simplify

take 0 _                    = []
take _ []                   = []
take n (x : xs) | n > 0     = x : take (n - 1) xs
                | otherwise = []

-}

take :: Int -> [a] -> [a]
take 0 _                    = []
take _ []                   = []
take n (x : xs) | n > 0     = x : take (n - 1) xs
                | otherwise = []

{-

last

1. define the type

last :: [a] -> a

2. enumerate the cases

last []
last [x]
last (x : xs)

3. define simple cases

last []       = error "empty"
last [x]      = x
last (x : xs) = ....

4. define other cases

last []       = error "empty"
last [x]      = x
last (x : xs) = last xs

5. generalize and simplify

last []       = error "empty"
last [x]      = x
last (x : xs) = last xs

-}

last :: [a] -> a
last []       = error "empty"
last [x]      = x
last (x : xs) = last xs
