{-# LANGUAGE ScopedTypeVariables #-}
module Utility
    (
    -- * Combinators
      y, owl
    -- * for digits
    , toDigits, fromDigits
    -- * for MonadPlus
    , whenP, ensure
    -- * for lists
    , chop, allSame, diff, lastn, removeSharedPrefix, odds, evens, sequentialGroupBy
    -- * for tuples
    , mapTuple
    -- * for boolean
    , implies
    -- * for Monad
    , concatMapM
    ) where

import Control.Applicative ( (<*>) )
import Control.Arrow       ( (***) )
import Control.Monad       ( guard, MonadPlus(mzero), liftM, join )
import Data.Bool.Extras    ( bool )
import Data.List           ( unfoldr )
import Data.Tuple          ( swap )

data Rec a = In { out :: Rec a -> a }

-- | Y combinator.
y :: (a -> a) -> a
{-# NOINLINE y #-}
y f = (\x -> f (out x x)) (In (\x -> f (out x x)))

-- | Combine an unary function and a binary function as @\\x y -> f (g x y) == owl f g@ .
owl :: (c -> d) -> (a -> b -> c) -> a -> b -> d
owl = (.).(.)

-- | Get digits of an integer.
toDigits :: Integral a => a -> [a]
toDigits = reverse . unfoldr step
  where
    step n = do
      guard $ n /= 0
      return $ swap $ divMod n 10

-- | Convert digits to an integer.
fromDigits :: Integral a => [a] -> a
fromDigits = foldl ((+) . (10*)) 0

-- | Capsulate value in `MonadPlus`.
whenP :: MonadPlus m => a -> Bool -> m a
whenP = bool mzero . return

-- | `whenP` with a predicate.
ensure :: MonadPlus m => (a -> Bool) -> a -> m a
ensure = (whenP <*>)

-- | Chop a list to lists of every @n@ elements.
chop :: Int -> [a] -> [[a]]
chop n = unfoldr $ fmap (splitAt n) . ensure (not . null)

-- | Return @True@ if the list has all the same elements.
allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (x==) xs

-- | Compute implication.
implies :: Bool -> Bool -> Bool
implies True False = False
implies _    _     = True

-- | `mapM` like concatMap.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

-- | Get rear part of the longer list.
diff :: [a] -> [a] -> [a]
diff [] ys = ys
diff xs [] = xs
diff (x:xs) (y:ys) = diff xs ys

-- | Get last @n@ elements of the list.
lastn :: Int -> [a] -> [a]
--lastn n xs = diff xs $ drop n xs
lastn = (diff <*>) . drop

-- | Apply f on both tuple elements.
mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple = join (***)

-- | Remove shared prefix from two lists.
removeSharedPrefix :: Eq a => [a] -> [a] -> ([a], [a])
removeSharedPrefix = (unzip .) . (dropWhile (uncurry (==)) .) . zip

-- | Return elements in odd positions.
odds :: [a] -> [a]
odds [] = []
odds (x:xs) = x : evens xs

-- | Return elements in even positions.
evens :: [a] -> [a]
evens [] = []
evens (_:xs) = odds xs

-- | Group consequent series.
--
-- >>> sequentialGroupBy (\x y -> succ x == y) [2,3,5,7,11,13,17,18,19]
-- [[2,3],[5],[7],[11],[13],[17,18,19]]
sequentialGroupBy :: forall a. (a -> a -> Bool) -> [a] -> [[a]]
sequentialGroupBy p = foldr f []
  where
    f :: a -> [[a]] -> [[a]]
    f x (ys@(y:_):yss) | p x y = (x:ys) : yss
    f x yss                    = [x]    : yss
