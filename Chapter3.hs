module Chapter3 where

{-

WAVE!!!

Types and Classes

Type - ia a collection of related values

type Bool has two values
True and False

repl> :i Bool
repl> :t True
repl> :t False

repl> :t not
repl> not True
repl> not False

<expression/value> :: <type>

Basic Types

Bool
True and False

Char
'a', 'A', '3', '_'

String
[Char]
"mike" "xyber"

Int
-9223372036854775808 to 9223372036854775807

Integer
no limit

Float
single-precision floating-point

Double
double-precision floating-point

List Types
a sequence of elements of the same type
can be of any length/size
[False, True, False] :: [Bool]
['a', 'b', 'c', 'd'] :: [Char] or String
"abcd"
["one", "two", "three"] :: [String] or [[Char]]

Tuple Types
sequence of elements of diffrent types
fixed length/size
(False, True) :: (Bool, Bool)
(False, 'a', 1) :: (Bool, Char, Int)
("Mike", True, 'a') = (String, Bool, Char)

Function Types
mapping of arguments of 1 type to the result of another/same type
not :: Bool -> Bool
isDigit :: Char -> Bool

WAVE!!!

Curried Functions
a function with n arguments return another function with n - 1 argument

-}

add :: Int -> Int -> Int
-- can also be expressed as Int -> (Int -> Int)
add x y = x + y
addTwo = add 2

{-

function add(x) {
    return function(y) {
        return x + y;
    };
}

OR

function add(x, y) {
    return x + y;
}

var addTwo = _.partial(add, 2);

-}

mult :: Int -> Int -> Int -> Int-> Int
-- Int -> (Int -> (Int -> Int))
mult x y z a = x * y * z * a
multThree = mult 3

{-

function mult(x) {
    return function(y) {
        return function(z) {
            return x * y * z;
        };
    };
}

OR

function mult(x, y ,z) {
    return x * y * z;
}

var multThree = _.partial(mult, 3)




Polymorphic Types
a type is polymorphic if there is more than one type it can have
repl> :t length
repl> :t head
repl> :t tail
repl> :t take
repl> :t zip



Overloaded Functions
functions that can perform same operation that has same type
repl> :i Num
repl> :t (+)
repl> :t (*)
repl> :t (-)
repl> :t negate
repl> :t abs
repl> :t signum


Basic Classes
collection of types that support certain overloaded functions
much like an Interface in OOP

Eq
equality types
repl> :i Eq
repl> :t (==)
repl> :t (/=)

Ord
oredered types
repl> :i Ord
repl> :t (>)
repl> :t (>=)
repl> :t (<)
repl> :t (<=)
repl> :t min
repl> :t max

Show
showable types
converts any value to a String
much like the toString() method in Java in its own Interface
repl> :i Show
repl> :t show

Read
readable types
reads a String converts to its proper type
repl> :i Read
repl> :t read

Num
numeric types
types that support basic numeric operations
repl> :i Num
repl> :t (+)
repl> :t (*)
repl> :t (-)
repl> :t negate
repl> :t abs
repl> :t signum

Integral
integral types
extends from Num, supports int division and integer remainder
repl> :i Integral

Fractional
frational types
also extends from Num, supports fractional division and reciprocation
repl> :i Fractional


-}
