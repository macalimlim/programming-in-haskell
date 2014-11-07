module Chapter5 where

{-

WAVE!!!

List Comprehensions

Similarites with Set Comprehensions/Builder Notation in Mathematics

-}

a = [ x ^ 2 | x <- [0..9] ]

b = [ (x, y) | x <- [1..3], y <- [4,5] ]

c = [ (x, y) | y <- [4, 5], x <- [1..3] ]

d = [ x * y | x <- [1..3], y <- [4,5] ]

e = [ (x, y) | x <- [1..3], y <- [x..5] ]

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

{-

Guards/Filters

-}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

{-

Prime Numbers
is a natural number greater than 1 that
has no positive divisors other than 1 and itself

-}

isPrime :: Int -> Bool
isPrime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], isPrime x]

find' :: Eq a => a -> [(a, b)] -> [b]
find' k xs = [s | (f, s) <- xs, f == k]

{-

The zip function

-}

zip' :: [a] -> [b] -> [(a, b)]
zip' [] []             = []
zip' [] (_ : _)        = []
zip' (_ : _) []        = []
zip' (x : xs) (y : ys) = (x, y) : zip xs ys

zip'' :: [a] -> [b] -> [(a, b)]
zip'' (x : xs) (y : ys) = (x, y) : zip'' xs ys
zip'' _ _               = []

{-

zip [1, 2, 3, 4, 5] ["mike", "rai", "rex"]
(1, "mike") : zip [2, 3, 4, 5] ["rai", "rex"]
(1, "mike") : ((2, "rai") : zip [3, 4, 5] ["rex"])
(1, "mike") : ((2, "rai") : ((3, "rex") : zip [4, 5] []))
(1, "mike") : ((2, "rai") : ((3, "rex") : []))

-}

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

{-

pairs [1, 2, 3, 4, 5]
zip [1, 2, 3, 4, 5] (tail [1, 2, 3, 4, 5])
zip [1, 2, 3, 4, 5] [2, 3, 4, 5]
(1, 2) : zip [2, 3, 4, 5] [3, 4, 5]
(1, 2) : (2, 3) : zip [3, 4, 5] [4, 5]
(1, 2) : (2, 3) : (3, 4) : zip [4, 5] [5]
(1, 2) : (2, 3) : (3, 4) : (4, 5) : zip [5] []
(1, 2) : (2, 3) : (3, 4) : (4, 5) : []

-}

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

{-

and [True, False, True]
True && and [False, True]
True && False && and [True]
True && False && True && and []
True && False && True && True

-}

isSorted :: Ord a => [a] -> Bool
isSorted xs = and' [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [s | (f, s) <- zip xs [0..(length xs - 1)], f == x]

{-

String Comprehensions

-}

f = "abcde" !! 2

g = take 3 "abcde"

h = length "abcde"

i = zip "abc" [1..4]

isLower :: Char -> Bool
isLower c = (c >= 'a') && (c <= 'z')

countLowers :: String -> Int
countLowers xs = length [x | x <- xs, isLower x]

countLetters :: Char -> String -> Int
countLetters c xs = length [x | x <- xs, x == c]

-- continue to Caesar Cipher
