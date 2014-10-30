module Chapter2 where

-- discuss Karate Kid Movie

-- discuss Prelude

-- repl: 2 + 3
-- repl: 2 - 3
-- repl: 2 * 3
-- repl: 7 `div` 2
-- repl: 2 ^ 3

-- emdas

-- repl: head [1, 2, 3, 4, 5]
-- repl: tail [1, 2, 3, 4, 5]
-- repl: [1, 2, 3, 4, 5] !! 2
-- repl: take 3 [1, 2, 3, 4, 5]
-- repl: drop 3 [1, 2, 3, 4, 5]
-- repl: length [1, 2, 3, 4, 5]
-- repl: sum [1, 2, 3, 4, 5]
-- repl: product [1, 2, 3, 4, 5]
-- repl: [1, 2, 3] ++ [4, 5]
-- repl: reverse [1, 2, 3, 4, 5]

-- repl: 1 `div` 0
-- repl: head []

-- discuss function application with accordance to mathematics
-- f(a, b) + c d
-- f a b + c * d
-- femdas
-- f a + b means (f a) + b

{-

Math
f(x)
f(x, y)
f(g(x))
f(x, g(y))
f(x)g(y)

Haskell
f x
f x y
f (g x)
f x (g y)
f x * g y

also compare with other languages

discuss working with a text editor and ghci
discuss common ghci commands

:load <name> or :l <name>
:reload or :r
:edit <name> :e <name>
:type <name> :t <name>
:?
:quit or :q

discuss naming requirements

functions starts with lowercase letters
but can be follwed by zero or more letters (upper and lower),
digits, underscores and quotes

you cannot use keywords as function names or values
case
class
data
default
deriving
do
else
if
import
in
infix
infixl
infixr
instance
let
module
newtype
of
then
type
where

discuss layout rule

-}

a = b + c
    where b = 1
          c = 2
d = a * 2

-- discuss comments (single line and multi line)

-- dicuss book, installation and fpcomplete
