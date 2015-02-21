-- | Main entry point to the application.
module Typeclassopedia where

import           Control.Applicative
import           Data.Char
import Prelude hiding (concat)
import Data.Monoid

-- ==================
-- Pair
-- ==================

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

-- ==================
-- Option
-- ==================

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

-- ==================
-- List
-- ==================

-- TODO make Foldable
data List a = Cons a (List a) | Nil deriving (Show)

list :: a -> List a
list x = Cons x Nil

cons :: a -> List a -> List a
x `cons` xs = Cons x xs
infixr 5 `cons`

append :: List a -> List a -> List a
Nil `append` ys = ys
(Cons x xs) `append` ys = x `cons` (xs `append` ys)

concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons xs Nil) = xs
concat (Cons xs ls) = xs `append` (concat ls)

-- *

instance Monoid (List a) where
    mappend = append
    mempty = Nil

-- * -> *

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

-- MAIN

main = do
    print $ concat (a `cons` b `cons` c `cons` Nil)
    print $ b `append` c
    print $ 1 `cons` mempty
    print $ a `mappend` b
    where
        a = 1 `cons` 2 `cons` Nil
        b = 3 `cons` 4 `cons` Nil
        c = 5 `cons` 6 `cons` Nil
