-- | Main entry point to the application.
module Typeclassopedia where

import           Control.Applicative
import           Data.Char
import Prelude hiding (concat)

-- Pair

data Pair a = Pair a a deriving (Show)

pair :: a -> Pair a
pair x = Pair x x

first :: Pair a -> a
first (Pair x _) = x

second :: Pair a -> a
second (Pair _ y) = y

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)
instance Applicative Pair where
    pure = pair
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)
-- Can't be a Monad?

-- Option

data Option a = Some a | None deriving (Show)

instance Functor Option where
    fmap _ None = None
    fmap f (Some x) = Some (f x)
instance Applicative Option where
    pure              = Some
    _ <*> None        = None
    None <*> _        = None
    Some f <*> Some x = Some (f x)
instance Monad Option where
    return         = Some
    (Some x) >>= k = k x
    None >>= _     = None

-- List

-- TODO make Foldable
data List a = Cons a (List a) | Nil deriving (Show)

list x = Cons x Nil

(<>) :: a -> List a -> List a
x <> xs = Cons x xs
infixr 9 <>

concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons (Cons x Nil) ls) = x <> concat ls
concat (Cons (Cons x xs) ls) = x <> concat (xs <> ls)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Applicative List where
    pure = list
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)
instance Monad List where
    return          = list
    Nil >>= _       = Nil
    xs >>= k        = concat $ fmap k xs

main = print $
    -- Dear reader, yes this is stupid but I'm playing around
    a >>= (\x -> x <> (x * x) <> Nil)
    where
        a = 1 <> 2 <> Nil
        b = 3 <> 4 <> Nil
        c = 5 <> 6 <> Nil
