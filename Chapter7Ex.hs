module Chapter7Ex where

{-

1. Show how the list comprehension [ f x | x <- xs, p x ] can be re-expressed
using the higher-order functions map and filter.

2. Without looking at the definitions from the standard prelude, define the
higher-order functions all , any , takeWhile , and dropWhile .

3. Redefine the functions map f and filter p using foldr .

4. Using foldl , define a function dec2int :: [ Int ] -> Int that converts a decimal
number into an integer. For example:
> dec2int [2, 3, 4, 5]
2345

5. Explain why the following definition is invalid:
sumsqreven = compose [sum, map (^2), filter even ]

-}

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id
