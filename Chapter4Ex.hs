module Chapter4Ex where

import           Prelude hiding ((&&), (||))

{-
1. Using library functions, define a function halve :: [ a ] -> ([ a ], [ a ]) that
splits an even-lengthed list into two halves. For example:
-}

halve :: [a] -> ([a], [a])
halve xs | even (length xs) = splitAt (length xs `div` 2) xs
         | otherwise        = error "odd number list"

{-
2. Consider a function safetail :: [ a ] → [ a ] that behaves as the library func-
tion tail , except that safetail maps the empty list to itself, whereas tail
produces an error in this case. Define safetail using:
a. conditional expression
b. guarded equation
c. pattern matching
Hint: make use of the library function null .
-}

tail' :: [a] -> [a]
tail' []       = error "empty list"
tail' (x : xs) = xs

-- conditional expression

safetail :: [a] -> [a]
safetail xs = if null xs
                 then []
                 else tail' xs

{-
safetail [1, 2, 3, 4, 5]
tail' [1, 2, 3, 4, 5]
[2, 3, 4, 5]

safetail []
[]
-}

-- guarded equation

safetail' :: [a] -> [a]
safetail' xs | null xs   = []
             | otherwise = tail' xs

{-
safetail [1, 2, 3, 4, 5]
tail' [1, 2, 3, 4, 5]
[2, 3, 4, 5]

safetail []
[]
-}

-- pattern matching

safetail'' :: [a] -> [a]
safetail'' []       = []
safetail'' (_ : xs) = xs

{-
safetail'' [1, 2, 3, 4, 5]
[2, 3, 4, 5]

safetail'' []
[]
-}

{-
3. In a similar way to &&, show how the logical disjunction operator || can be
defined in four different ways using pattern matching.
-}

(||) :: Bool -> Bool -> Bool
True || True   = True
True || False  = True
False || True  = True
False || False = False

{-
4. Redefine the following version of the conjunction operator using conditional
expressions rather than pattern matching:
True && True = True
_ && _       = False
-}

(&&) :: Bool -> Bool -> Bool
x && y = if x == True
            then if y == True
                    then True
                    else False
            else False

{-
5. Do the same for the following version, and note the difference in the number
of conditional expressions required:
True && b  = b
False && _ = False
-}

(&&&) :: Bool -> Bool -> Bool
x &&& y = if x == True
             then y
             else False

{-
6. Show how the curried function definition mult x y z = x ∗ y ∗ z can be
understood in terms of lambda expressions.
-}

mult :: Num a => a -> a -> a -> a
mult x y z = x * y * z

mult' :: Num a => a -> a -> a -> a
mult' y z = \ x -> x * y * z

mult'' :: Num a => a-> a -> a -> a
mult'' z = \ x -> \ y -> x * y * z

mult''' :: Num a => a -> a -> a -> a
mult''' = \ x -> \ y -> \ z -> x * y * z

{-

function mult(x, y, z) {
    return x * y * z;
}


function mult(y, z) {
    return function(x) {
        return x * y * z;
    };
}

function mult(z) {
    return function(x) {
        return function(y) {
            return x * y * z;
        };
    };
}

function mult() {
    return function(x) {
        return function(y) {
            return function(z) {
                return x * y * z;
            };
        };
    };
}

-}
