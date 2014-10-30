module Scratch where

import           Prelude hiding ((&&), (||))

f :: (a -> a) -> a
f g = undefined

remove :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop (n + 1) xs

funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs

e3 x = x * 2

e4 (x, y) = x

e6 x y = x * y

e10 (x, y) = [x, y]

e11 :: (Char, Bool)
e11 = ('\a', True)

e13 :: Int -> Int -> Int
e13 x y = x + y * y
