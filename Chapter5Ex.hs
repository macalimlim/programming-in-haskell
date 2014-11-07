module Chapter5Ex where

{-

1. Using a list comprehension, give an expression that calculates the sum
1 ^ 2 + 2 ^ 2 + . . . 100 ^ 2 of the first one hundred integer squares.

-}

sumOf100 = sum [x ^ 2 | x <- [1..100]]

{-

2. In a similar way to the function length , show how the library function
replicate :: Int → a → [ a ] that produces a list of identical elements can
be defined using a list comprehension. For example:
> replicate 3 True
[ True , True , True ]

-}

lcReplicate :: Int -> a -> [a]
lcReplicate n x = [x | _ <- [1..n]]

{-

3. A triple (x, y, z) of positive integers is pythagorean if x ^ 2 + y ^ 2 = z ^ 2 . Using
a list comprehension, define a function pyths :: Int → [( Int , Int , Int )] that
returns the list of all pythagorean triples whose components are at most a
given limit. For example:
> pyths 10
[(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

-}

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

{-

4. A positive integer is perfect if it equals the sum of its factors, excluding the
number itself. Using a list comprehension and the function factors , define a
function perfects :: Int → [ Int ] that returns the list of all perfect numbers
up to a given limit. For example:
> perfects 500
[6, 28, 496]

-}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPerfect :: Int -> Bool
isPerfect n = let facs = factors n
              in sum (init facs) == last facs

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]

{-

5. Show how the single comprehension [(x, y) | x <- [1, 2, 3], y <-
[4, 5, 6]] with two generators can be re-expressed using two comprehen-
sions with single generators. Hint: make use of the library function concat
and nest one comprehension within the other.

-}

comp2 :: [(Int, Int)]
comp2 = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

{-

6. Redefine the function positions using the function find.

-}

find :: Eq a => a -> [(a, b)] -> [b]
find k ps = [s | (f, s) <- ps, f == k]

positions :: Eq a => a -> [a] -> [Int]
positions k xs = find k (zip xs [0..(length xs - 1)])

{-

7. The scalar product of two lists of integers xs and ys of length n is given by
the sum of the products of corresponding integers:
[Please see the formula in the book]
In a similar manner to the function chisqr , show how a list comprehension
can be used to define a function scalarproduct :: [ Int ] → [ Int ] → Int that
returns the scalar product of two lists. For example:
> scalarproduct [1, 2, 3] [4, 5, 6]
32

-}

-- Scalar Product is also known as Dot Product

dotp :: [Int] -> [Int] -> Int
dotp xs ys = sum [x * y | (x, y) <- zip xs ys]

{-

8. Modify the Caesar cipher program to also handle upper-case letters.

-}


{-

9. Crack it :D

-}
