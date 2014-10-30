module Chapter1 where

-- what is functional programming?
-- immutability
-- highr order functions
-- recursion
-- purity
-- lazy evaluation

{-
other fp languages (or supports fp)
erlang (elixir)
lisp (scheme, racket, clojure, elisp)
ml
f#
scala
javascript
php
perl
java 8
python
-}

double x = x + x

{-
2 types of evaluation
  eager/strict (call by value)
  lazy (call by name)

show Java version of summing 1 to 10
show Haskell version of summing 1 to 10
repl: sum [1..10]

Features of Haskell
1. Concise programs
2. Powerful Type System
3. List Comprehensions
4. Recursive Functions
5. Higher-order Functions
6. Monadic Effects
7. Lazy Evaluation
8. Reasoning about programs

Historical Background
1930s Alonzo Church
      Lambda Calculus - a system for mathematical computation
      using function abstraction and application
1950s John McCarthy Lisp (LISt Processor)
      1st fp language (with mutable data)
1960s Peter Landin ISWIM (If you See What I Mean)
      1st 'pure' fp language (no assignments)
1970s John Backus FP (Functional Programming)
      extensive use of higher order functions
1970s Robin Milner ML (Meta-Language)
      fp with type inference and polymorphic types (generics)
1970s and 1980s David Turner Miranda
      developed a number of lazy fp languages
1987 Simon Peyton Jones, Paul Hudak, Philip Wadler, John Hughes, et al.
     Haskell
2003 Haskell 98 Report
     1st stable version

Use in the Industry
Google
Facebook
Twitter
League of Legends
Call of Duty
Eve Online

-}

sum' []       = 0
sum' (x : xs) = x + sum' xs

qsort []       = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
                 where
                        smaller = [l | l <- xs, l <= x]
                        larger  = [r | r <- xs, r > x]

{-
install ghc

install development environment
        emacs
        vi
        eclipsefp
        fpcomplete

ghci interpreter
http://tryhaskell.org/

-}
