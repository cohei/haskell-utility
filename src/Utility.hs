{-# LANGUAGE ScopedTypeVariables #-}
module Utility
    (
    -- * Combinators
      y, owl
    -- * for digits
    , toDigits, fromDigits
    -- * for Alternative
    , whenA, ensure
    -- * for lists
    , chop, allSame, diff, lastn, removeSharedPrefix, odds, evens, sequentialGroupBy
    -- * for tuples
    , mapTuple
    -- * for boolean
    , implies
    ) where

import           Control.Applicative (Alternative (empty), (<*>))
import           Control.Arrow       ((***))
import           Control.Monad       (guard, join)
import           Data.Bool           (bool)
import           Data.List           (unfoldr)
import           Data.Tuple          (swap)

data Rec a = In { out :: Rec a -> a }

-- | Y combinator.
y :: (a -> a) -> a
{-# NOINLINE y #-}
y f = (\x -> f (out x x)) (In (\x -> f (out x x)))

-- | Combine an unary function and a binary function as @\\x y -> f (g x y) == owl f g@ .
owl :: (c -> d) -> (a -> b -> c) -> a -> b -> d
owl = (.).(.)

-- | Get digits of an integer.
--
-- >>> toDigits 1234
-- [1,2,3,4]
toDigits :: Integral a => a -> [a]
toDigits = reverse . unfoldr step
  where
    step n = do
      guard $ n /= 0
      return $ swap $ divMod n 10

-- | Convert digits to an integer.
--
-- >>> fromDigits [1, 2, 3, 4]
-- 1234
fromDigits :: Integral a => [a] -> a
fromDigits = foldl ((+) . (10*)) 0

-- | Capsulate value in `Alternative`.
whenA :: Alternative f => a -> Bool -> f a
whenA = bool empty . pure

-- | `whenA` with a predicate.
ensure :: Alternative m => (a -> Bool) -> a -> m a
ensure = (whenA <*>)

-- | Chop a list to lists of every @n@ elements.
chop :: Int -> [a] -> [[a]]
chop n = unfoldr $ fmap (splitAt n) . ensure (not . null)

-- | Return @True@ if the list has all the same elements.
allSame :: Eq a => [a] -> Bool
allSame []     = True
allSame (x:xs) = all (x==) xs

-- | Compute implication.
implies :: Bool -> Bool -> Bool
implies True False = False
implies _    _     = True

-- | Get rear part of the longer list.
diff :: [a] -> [a] -> [a]
diff [] ys         = ys
diff xs []         = xs
diff (_:xs) (_:ys) = diff xs ys

-- | Get last @n@ elements of the list.
lastn :: Int -> [a] -> [a]
--lastn n xs = diff xs $ drop n xs
lastn = (diff <*>) . drop

-- | Apply f on both tuple elements.
--
-- >>> mapTuple (+ 1) (3, 4)
-- (4,5)
mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple = join (***)

-- | Remove shared prefix from two lists.
removeSharedPrefix :: Eq a => [a] -> [a] -> ([a], [a])
removeSharedPrefix = (unzip .) . (dropWhile (uncurry (==)) .) . zip

-- | Return elements in odd positions.
--
-- >>> odds [1..10]
-- [1,3,5,7,9]
odds :: [a] -> [a]
odds []     = []
odds (x:xs) = x : evens xs

-- | Return elements in even positions.
--
-- >>> evens [1..10]
-- [2,4,6,8,10]
evens :: [a] -> [a]
evens []     = []
evens (_:xs) = odds xs

-- | Group consequent series.
--
-- >>> sequentialGroupBy (\x y -> succ x == y) [2,3,5,7,11,13,17,18,19]
-- [[2,3],[5],[7],[11],[13],[17,18,19]]
sequentialGroupBy :: forall a. (a -> a -> Bool) -> [a] -> [[a]]
sequentialGroupBy p = foldr f []
  where
    f :: a -> [[a]] -> [[a]]
    f x (ys@(y':_):yss) | p x y' = (x:ys) : yss
    f x yss             = [x]    : yss
