{-# LANGUAGE NoImplicitPrelude #-}

module Monads where

import           Prelude (Int, Show, String)

{-

Monads

- A monad is a way to structure computations in terms of values and sequences of computations using those values. Monads allow the programmer to build up computations using sequential building blocks, which can themselves be sequences of computations.

- its just another design pattern



It basically follows these 3 functions

return :: Monad m => a -> m a
bind :: Monad m => m a -> (a -> m b) -> m b
-- the other function is the one passed to bind

It must abide by these 3 laws

Left identity

Right Identity

Associativity

-}

class Monad m where
      return :: a -> m a
      bind :: m a -> (a -> m b) -> m b

-- Identity Monad

data Id a = Id a
            deriving Show

instance Monad Id where
         return x = Id x
         (Id x) `bind` f = f x

-- Maybe Monad

data Maybe a = Nothing | Just a
               deriving Show

instance Monad Maybe where
         return x = Just x
         (Just x) `bind` f = f x
         Nothing `bind` _  = Nothing

-- Either Monad

data Either a b = Left a | Right b
                  deriving Show

instance Monad (Either a) where
         return x = Right x
         (Right x) `bind` f = f x
         (Left x) `bind` _  = Left x
