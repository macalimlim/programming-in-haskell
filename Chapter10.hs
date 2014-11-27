module Chapter10 where

import           Prelude hiding (Left, Right, String)

{-

Types and Classes


Type declarations
declaring type synonyms

-}

type String = [Char]

type Pos = (Int, Int)
type Board = [Pos]

-- it can also be parameterised

type Assoc k v = [(k, v)]

{-

Data declarations

data Bool = False | True

-}


data Move = Left | Right | Up | Down
            deriving Show

move :: Move -> Pos -> Pos
move Left (x, y)  = (x - 1, y)
move Right (x, y) = (x + 1, y)
move Up (x, y)    = (x, y + 1)
move Down (x, y)  = (x, y - 1)

moves :: [Move] -> Pos -> Pos
moves [] p       = p
moves (m : ms) p = moves ms (move m p)


flip :: Move -> Move
flip Left  = Right
flip Right = Left
flip Up    = Down
flip Down  = Up


data Shape = Circle Float | Rect Float Float
             deriving Show

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

{-

Recursive types

-}

data Nat = Zero | Succ Nat
           deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)



data List a = Nil | Cons a (List a)
              deriving Show

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs



data Tree a = Leaf a | Branch (Tree a) a (Tree a)
              deriving Show

t :: Tree Int
t = Branch (Branch (Leaf 1) 3 (Leaf 4)) 5 (Branch (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs m (Leaf n)     = m == n
occurs m (Branch l n r) = m == n || occurs m l || occurs m r

-- if the tree is ordered

occurs' :: Ord a => a -> Tree a -> Bool
occurs' m (Leaf n)                   = m == n
occurs' m (Branch l n r) | m == n    = True
                         | m < n     = occurs' m l
                         | otherwise = occurs' m r

flatten :: Tree a -> [a]
flatten (Leaf n)       = [n]
flatten (Branch l n r) = flatten l ++ [n] ++ flatten r
