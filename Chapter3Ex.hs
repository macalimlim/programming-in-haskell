module Chapter3Ex where

{-
1. What are the types of the following values?

['a', 'b', 'c']
[Char] or String

('a', 'b', 'c')
(Char, Char, Char)

[(False, '0'), (True, '1')]
[(Bool, Char)]

([False, True], ['0', '1'])
([Bool], [Char])
([Bool], String)

[tail, init, reverse]
[[a] -> [a]]


2. What are the types of the following functions?
-}

tail' :: [a] -> [a]
tail' [] = error "empty"
tail' (x : xs) = xs

head' :: [a] -> a
head' [] = error "empty"
head' (x : xs) = x

second xs = head' (tail' xs)
-- second :: [a] -> a

swap (x, y) = (y, x)
-- swap :: (a, b) -> (b, a)

pair x y = (x, y)
-- pair :: a -> b -> (a, b)

double x = x * 2
-- double :: Num a => a -> a

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse xs ++ [x]

palindrome xs = reverse' xs == xs
-- palindrome :: Eq a => [a] -> Bool

twice f x = f (f x)
-- twice :: (a -> b) -> a -> b

{-

3. Check your answers to the preceding two questions using ghci.

4. Why is it not feasible in general for function types to be instances of the Eq
class? When is it feasible? Hint: two functions of the same type are equal if
they always return equal results for equal arguments.

Why?
Because of the Halting Problem

When?
If someone solved the Halting Problem

-}

-- considering a machine with infinite resources and not affected by external factors

fnHaltsImmediately :: Int -> Int
fnHaltsImmediately n = n + 3

fnHaltsForSomeTime :: Int -> Int
fnHaltsForSomeTime n | (foldl (+) 0 [1..n]) > 0 = n + 3
                     | otherwise                = n + 3

fnDoesNotHalt :: Int -> Int
fnDoesNotHalt n = fnDoesNotHalt n

willFnHalt :: (Int -> Int) -> Int -> Bool
willFnHalt f n | f n > 0   = True
               | otherwise = True
