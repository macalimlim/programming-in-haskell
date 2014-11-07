module Chapter7 where

import           Prelude hiding (and, filter, foldl, foldr, length, map, odd,
                          or, product, reverse, sum, (.))

{-

Higher Order Functions

encapsulates common code into functions
reusability
DRY - don't repeat yourself

-}

add :: Int -> Int -> Int
add x y = x + y

--means

add' :: Int -> (Int -> Int)
add' = \ x -> (\ y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

qsort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
qsort _ []       = []
qsort f (x : xs) = qsort f left ++ [x] ++ qsort f right
                   where left = [l | l <- xs, f l x || l == x]
                         right = [r | r <- xs, not (f r x)]

fqsort :: Ord a => [a] -> [a]
fqsort = qsort (<)

rqsort :: Ord a => [a] -> [a]
rqsort = qsort (>)

insert :: Ord a => (a -> a -> Bool) -> a -> [a] -> [a]
insert _ x []                         = [x]
insert f x (y : ys) | f x y || x == y = x : y : ys
                    | otherwise       = y : insert f x ys

finsert :: Ord a => a -> [a] -> [a]
finsert = insert (<)

rinsert :: Ord a => a -> [a] -> [a]
rinsert = insert (>)

isort :: Ord a => (a -> [a] -> [a]) -> [a] -> [a]
isort _ []       = []
isort f (x : xs) = f x (isort f xs)

fisort :: Ord a => [a] -> [a]
fisort = isort finsert

risort :: Ord a => [a] -> [a]
risort = isort rinsert

{-

Processing Lists

-}

-- The map function

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs

{-

map (+1) [1, 3, 5, 7]
(+1) 1 : map (+1) [3, 5, 7]
(+1) 1 : ((+1) 3 : map (+1) [5, 7])
(+1) 1 : ((+1) 3 : ((+1) 5 : map (+1) [7]))
(+1) 1 : ((+1) 3 : ((+1) 5 : ((+1) 7 : map (+1) [])))
(+1) 1 : ((+1) 3 : ((+1) 5 : ((+1) 7 : [])))
(+1) 1 : ((+1) 3 : ((+1) 5 : (8 : [])))
(+1) 1 : ((+1) 3 : ((+1) 5 : [8]))
(+1) 1 : ((+1) 3 : (6 : [8] ))
(+1) 1 : ((+1) 3 : [6, 8])
(+1) 1 : (4 : [6, 8])
(+1) 1 : [4, 6, 8]
2 : [4, 6, 8]
[2, 4, 6, 8]

map isDigit ['a', '1', 'b', '2']
isDigit 'a' : map isDigit ['1', 'b', '2']
isDigit 'a' : (isDigit '1' : map isDigit ['b', '2'])
isDigit 'a' : (isDigit '1' : (isDigit 'b' : map isDigit ['2']))
isDigit 'a' : (isDigit '1' : (isDigit 'b' : (isDigit '2' : map isDigit [])))
isDigit 'a' : (isDigit '1' : (isDigit 'b' : (isDigit '2' : [])))
isDigit 'a' : (isDigit '1' : (isDigit 'b' : (True : [])))
isDigit 'a' : (isDigit '1' : (isDigit 'b' : [True]))
isDigit 'a' : (isDigit '1' : (False : [True]))
isDigit 'a' : (isDigit '1' : [False, True])
isDigit 'a' : (True : [False, True])
isDigit 'a' : [True, False, True]
False : [True, False, True]
[False, True, False, True]

map reverse ["abc", "def", "ghi"]
reverse "abc" : map reverse ["def", "ghi"]
reverse "abc" : (reverse "def" : map reverse ["ghi"])
reverse "abc" : (reverse "def" : (reverse "ghi" : map reverse []))
reverse "abc" : (reverse "def" : (reverse "ghi" : []))
reverse "abc" : (reverse "def" : ("ihg" : []))
reverse "abc" : (reverse "def" : ["ihg"])
reverse "abc" : ("fed" : ["ihg"])
reverse "abc" : ["fed", "ihg"]
"cba" : ["fed", "ihg"]
["cba", "fed", "ihg"]

-}

-- The filter function

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []       = []
filter' f (x : xs) | f x       = x : filter' f xs
                   | otherwise = filter' f xs

{-

filter even [1..10]
filter even [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
filter even [2, 3, 4, 5, 6, 7, 8, 9, 10]
2 : filter even [3, 4, 5, 6, 7, 8, 9, 10]
2 : filter even [4, 5, 6, 7, 8, 9, 10]
2 : (4 : filter even [5, 6, 7, 8, 9, 10])
2 : (4 : filter even [6, 7, 8, 9, 10])
2 : (4 : (6 : filter even [7, 8, 9, 10]))
2 : (4 : (6 : filter even [8, 9, 10]))
2 : (4 : (6 : (8 : filter even [9, 10])))
2 : (4 : (6 : (8 : filter even [10])))
2 : (4 : (6 : (8 : (10 : filter even []))))
2 : (4 : (6 : (8 : (10 : []))))
2 : (4 : (6 : (8 : [10])))
2 : (4 : (6 : [8, 10]))
2 : (4 : [6, 8, 10])
2 : [4, 6, 8, 10]
[2, 4, 6, 8, 10]

filter (>5) [1..10]
filter (>5) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
filter (>5) [2, 3, 4, 5, 6, 7, 8, 9, 10]
filter (>5) [3, 4, 5, 6, 7, 8, 9, 10]
filter (>5) [4, 5, 6, 7, 8, 9, 10]
filter (>5) [5, 6, 7, 8, 9, 10]
filter (>5) [6, 7, 8, 9, 10]
6 : filter (>5) [7, 8, 9, 10]
6 : (7 : filter (>5) [8, 9, 10])
6 : (7 : (8 : filter (>5) [9, 10]))
6 : (7 : (8 : (9 : filter (>5) [10])))
6 : (7 : (8 : (9 : (10 : filter (>5) []))))
6 : (7 : (8 : (9 : (10 : []))))
6 : (7 : (8 : (9 : [10])))
6 : (7 : (8 : [9, 10]))
6 : (7 : [8, 9, 10])
6 : [7, 8, 9, 10]
[6, 7, 8, 9, 10]

filter (/= ' ') "abc def ghi"
'a' : filter (/= ' ') "bc def ghi"
'a' : ('b' : filter (/= ' ') "c def ghi")
'a' : ('b' : ('c' : filter (/= ' ') " def ghi"))
'a' : ('b' : ('c' : filter (/= ' ') "def ghi"))
'a' : ('b' : ('c' : ('d' : filter (/= ' ') "ef ghi")))
'a' : ('b' : ('c' : ('d' : ('e' : filter (/= ' ') "f ghi"))))
'a' : ('b' : ('c' : ('d' : ('e' : ('f' : filter (/= ' ') " ghi")))))
'a' : ('b' : ('c' : ('d' : ('e' : ('f' : filter (/= ' ') "ghi")))))
'a' : ('b' : ('c' : ('d' : ('e' : ('f' : ('g' : filter (\= ' ') "hi"))))))
'a' : ('b' : ('c' : ('d' : ('e' : ('f' : ('g' : ('h' : filter (/= ' ') "i")))))))
'a' : ('b' : ('c' : ('d' : ('e' : ('f' : ('g' : ('h' : ('i' : filter (/= ' ') []))))))))
'a' : ('b' : ('c' : ('d' : ('e' : ('f' : ('g' : ('h' : ('i' : []))))))))
'a' : ('b' : ('c' : ('d' : ('e' : ('f' : ('g' : ('h' : ['i'])))))))
'a' : ('b' : ('c' : ('d' : ('e' : ('f' : ('g' : ['h', 'i']))))))
'a' : ('b' : ('c' : ('d' : ('e' : ('f' : ['g', 'h', 'i'])))))
'a' : ('b' : ('c' : ('d' : ('e' : ['f', 'g', 'h', 'i']))))
'a' : ('b' : ('c' : ('d' : ['e', 'f', 'g', 'h', 'i'])))
'a' : ('b' : ('c' : ['d', 'e', 'f', 'g', 'h', 'i']))
'a' : ('b' : ['c', 'd', 'e', 'f', 'g', 'h', 'i'])
'a' : ['b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']
['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']
"abcdefghi"

-}

-- The foldr function

length :: [Int] -> Int
length []       = 0
length (x : xs) = 1 + length xs

sum :: [Int] -> Int
sum []       = 0
sum (x : xs) = x + sum xs

product :: [Int] -> Int
product []       = 1
product (x : xs) = x * product xs

or :: [Bool] -> Bool
or []       = False
or (x : xs) = x || or xs

and :: [Bool] -> Bool
and []       = True
and (x : xs) = x && and xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

{-

The pattern

f [] = v
f (x : xs) = x [*] f xs

-}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v []       = v
foldr f v (x : xs) = f x (foldr f v xs)



add1ToSeed :: a -> Int -> Int
add1ToSeed _ s = 1 + s

length' :: [a] -> Int
length' = foldr add1ToSeed 0

{-

length [1, 2, 3]
foldr add1ToSeed 0 [1, 2, 3]
add1ToSeed 1 (foldr add1ToSeed 0 [2, 3])
add1ToSeed 1 (add1ToSeed 2 (foldr add1ToSeed 0 [3]))
add1ToSeed 1 (add1ToSeed 2 (add1ToSeed 3 (foldr add1ToSeed 0 [])))
add1ToSeed 1 (add1ToSeed 2 (add1ToSeed 3 0))
add1ToSeed 1 (add1ToSeed 2 (1 + 0))
add1ToSeed 1 (add1ToSeed 2 1)
add1ToSeed 1 (1 + 1)
add1ToSeed 1 2
1 + 2
3

-}

sum' :: [Int] -> Int
sum' = foldr (+) 0

{-

sum [1, 2, 3]
foldr (+) 0 [1, 2, 3]
(+) 1 (foldr (+) 0 [2, 3])
(+) 1 ((+) 2 (foldr (+) 0 [3]))
(+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [])))
(+) 1 ((+) 2 ((+) 3 0))
1 + (2 + (3 + 0))
1 + (2 + 3)
1 + 5
6

-}

product' :: [Int] -> Int
product' = foldr (*) 1

{-

product [1, 2, 3]
foldr (*) 1 [1, 2, 3]
(*) 1 (foldr (*) 1 [2, 3])
(*) 1 ((*) 2 (foldr (*) 1 [3]))
(*) 1 ((*) 2 ((*) 3 (foldr (*) 1 [])))
(*) 1 ((*) 2 ((*) 3 1))
1 * (2 * (3 * 1))
1 * (2 * 3)
1 * 6
6

-}

or' :: [Bool] -> Bool
or' = foldr (||) False

{-

or [True, False, True]
foldr (||) False [True, False, True]
(||) True (foldr (||) False [False, True])
(||) True ((||) False (foldr (||) False [True]))
(||) True ((||) False ((||) True (foldr (||) False [])))
(||) True ((||) False ((||) True False))
True || (False || (True || False))
True || (False || True)
True || True
True

-}

and' :: [Bool] -> Bool
and' = foldr (&&) True

{-

and [True, False, True]
foldr (&&) True [True, False, True]
(&&) True (foldr (&&) True [False, True])
(&&) True ((&&) False (foldr (&&) True [True]))
(&&) True ((&&) False ((&&) True (foldr (&&) True [])))
(&&) True ((&&) False ((&&) True True))
True && (False && (True && True))
True && (False && True)
True && False
False

-}

snoc :: a -> [a] -> [a]
snoc i s = s ++ [i]

reverse' :: [a] -> [a]
reverse' = foldr snoc []

{-

reverse [1, 2, 3]
foldr snoc [] [1, 2, 3]
snoc 1 (foldr snoc [] [2, 3])
snoc 1 (snoc 2 (foldr snoc [] [3]))
snoc 1 (snoc 2 (snoc 3 (foldr snoc [] [])))
snoc 1 (snoc 2 (snoc 3 []))
snoc 1 (snoc 2 ([] ++ [3]))
snoc 1 (snoc 2 [3])
snoc 1 ([3] ++ [2])
snoc 1 [3, 2]
[3, 2] ++ [1]
[3, 2, 1]

-}

-- The foldl function

length'' :: [a] -> Int
length'' = length_ 0
           where length_ v []       = v
                 length_ v (x : xs) = length_ (v + 1) xs

{-

length [1, 2, 3]
length_ (0 + 1) [2, 3]
length_ ((0 + 1) + 1) [3]
length_ (((0 + 1) + 1) + 1) []
(((0 + 1) + 1) + 1)
((1 + 1) + 1)
(2 + 1)
3

-}

sum'' :: [Int] -> Int
sum'' = sum_ 0
        where sum_ v []       = v
              sum_ v (x : xs) = sum_ (v + x) xs

{-

sum [1, 2, 3]
sum_ (0 + 1) [2, 3]
sum_ ((0 + 1) + 2) [3]
sum_ (((0 + 1) + 2) + 3) []
(((0 + 1) + 2) + 3)
((1 + 2) + 3)
(3 + 3)
6

-}

product'' :: [Int] -> Int
product'' = product_ 1
            where product_ v []       = v
                  product_ v (x : xs) = product_ (v * x) xs

{-

product [1, 2, 3]
product_ (1 * 1) [2, 3]
product_ ((1 * 1) * 2) [3]
product_ (((1 * 1) * 2) * 3) []
(((1 * 1) * 2) * 3)
((1 * 2) * 3)
(2 * 3)
6

-}

or'' :: [Bool] -> Bool
or'' = or_ False
       where or_ v []       = v
             or_ v (x : xs) = or_ (v || x) xs

{-

or [True, False, True]
or_ (False || True) [False, True]
or_ ((False || True) || False) [True]
or_ (((False || True) || False) || True) []
(((False || True) || False) || True)
((True || False) || True)
(True || True)
True

-}

and'' :: [Bool] -> Bool
and'' = and_ True
        where and_ v []       = v
              and_ v (x : xs) = and_ (v && x) xs

{-

and [True, False, True]
and_ (True && True) [False, True]
and_ ((True && True) && False) [True]
and_ (((True && True) && False) && True) []
(((True && True) && False) && True)
((True && False) && True)
(False && True)
False

-}

reverse'' :: [a] -> [a]
reverse'' = rev_ []
            where rev_ v []       = v
                  rev_ v (x : xs) = rev_ ([x] ++ v) xs

{-

reverse [1, 2, 3]
rev_ ([1] ++ []) [2, 3]
rev_ ([2] ++ ([1] ++ [])) [3]
rev_ ([3] ++ ([2] ++ ([1] ++ []))) []
([3] ++ ([2] ++ ([1] ++ [])))
([3] ++ ([2] ++ [1]))
([3] ++ [2, 1])
[3, 2, 1]

-}

{-

The pattern

f v []       = v
f v (x : xs) = f (v [*] x) xs

-}

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ v []       = v
foldl f v (x : xs) = foldl f (f v x) xs

length''' :: [a] -> Int
length''' = foldl (\ s _ -> 1 + s) 0

sum''' :: [Int] -> Int
sum''' = foldl (+) 0

product''' :: [Int] -> Int
product''' = foldl (*) 1

or''' :: [Bool] -> Bool
or''' = foldl (||) False

and''' :: [Bool] -> Bool
and''' = foldl (&&) True

reverse''' :: [a] -> [a]
reverse''' = foldl (\ s i -> [i] ++ s) []

{-

The composition operator

-}

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \ x -> f (g x)

{-

function compose(f, g) {
    return function(x) {
        return f(g(x));
    };
}

-}

notNull :: [a] -> Bool
notNull = not . null

odd :: Int -> Bool
odd = not . even

twice' :: (a -> a) -> a -> a
twice' f = f . f

sumSqrEven :: [Int] -> Int
sumSqrEven = sum . map (+10) . filter even
