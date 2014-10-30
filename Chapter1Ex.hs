module Chapter1Ex where

-- 1. Give another possible calculation for the result of double (double 2).

double x = x + x

{-

double (double 2)
double (2 + 2)
(2 + 2) + (2 + 2)
4 + (2 + 2)
4 + 4
8

-}

-- 2. Show that sum [x] = x for any number x.

sum' []       = 0
sum' (x : xs) = x + sum xs

{-

sum [8, 15, 76, 81, 5]
8 + sum [15, 76, 81, 5]
8 + (15 + sum [76, 81, 5])
8 + (15 + (76 + sum [81, 5]))
8 + (15 + (76 + (81 + sum [5])))
8 + (15 + (76 + (81 + (5 + sum []))))
8 + (15 + (76 + (81 + (5 + (0)))))
185

-}

-- 3. Define a function product that produces the product of a list of numbers, and show using your definition that product [2, 3, 4] = 24.

product' []       = 1
product' (x : xs) = x * product xs

{-

product [2, 3, 4]
2 * product [3, 4]
2 * (3 * product [4])
2 * (3 * (4 * product []))
2 * (3 * (4 * (1)))
24

-}

-- 4. How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?

qsort []       = []
qsort (x : xs) = qsort left ++ [x] ++ qsort right
                 where left  = [l | l <- xs, l <= x]
                       right = [r | r <- xs, r > x]

rqsort []       = []
rqsort (x : xs) = rqsort left ++ [x] ++ rqsort right
                  where left  = [l | l <- xs, l >= x]
                        right = [r | r <- xs, r < x]

{-

qsort [2, 9, 1, 3, 10]
(qsort [1]) ++ [2] ++ (qsort [9, 3, 10])
([] ++ [1] ++ []) ++ [2] ++ ((qsort [3]) ++ [9] ++ (qsort [10]))
([] ++ [1] ++ []) ++ [2] ++ (([] ++ [3] ++ []) ++ [9] ++ ([] ++ [10] ++ []))

-}

-- 5. What would be the effect of replacing <= by < in the definition of qsort? Hint: consider the example qsort [2, 2, 3, 1, 1].

xqsort []       = []
xqsort (x : xs) = xqsort left ++ [x] ++ xqsort right
                  where left  = [l | l <- xs, l < x]
                        right = [r | r <- xs, r > x]

{-

xqsort [2, 2, 3, 1, 1]
(xqsort [1]) ++ [2] ++ (xqsort [3])
([] ++ [1] ++ []) ++ [2] ++ ([] ++ [3] ++ [])

-}

-- elements would be sorted, but some duplicate elements will be omitted
