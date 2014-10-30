module CaesarCipher where

import           Data.Char hiding (isLower, isUpper)

{-

a Caesar cipher, also known as the shift cipher, Caesar's code or Caesar shift,
is one of the simplest and most widely known encryption techniques.
It is a type of substitution cipher in which each letter in the plaintext
is replaced by a letter some fixed number of positions down the alphabet.
For example, with a left shift of 3, D would be replaced by A, E
would become B, and so on.
The method is named after Julius Caesar, who used it in his private correspondence.

"haskell is fun"
with a shift factor of 3
"kdvnhoo lv ixq"
shift factor of 10
"rkcuovv sc pex"

-}

isInRange :: Char -> Char -> Char -> Bool
isInRange l u c = (c >= l) && (c <= u)

isLower :: Char -> Bool
isLower = isInRange 'a' 'z'

isUpper :: Char -> Bool
isUpper = isInRange 'A' 'Z'

--

let2Int :: Char -> Char -> Int
let2Int x c = ord c - ord x

lcLet2Int :: Char -> Int
lcLet2Int = let2Int 'a'

ucLet2Int :: Char -> Int
ucLet2Int = let2Int 'A'

--

int2Let :: Char -> Int -> Char
int2Let x n = chr (ord x + n)

int2LcLet :: Int -> Char
int2LcLet = int2Let 'a'

int2UcLet :: Int -> Char
int2UcLet = int2Let 'A'

--

shift :: Int -> Char -> Char
shift n c | isLower c = int2LcLet ((lcLet2Int c + n) `mod` 26)
          | isUpper c = int2UcLet ((ucLet2Int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n = encode (-n)

-- Assignment: how to crack it! :D
