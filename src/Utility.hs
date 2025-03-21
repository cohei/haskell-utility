{-# LANGUAGE ScopedTypeVariables #-}

module Utility
  ( -- * Combinators
    y,
    owl,

    -- * for numbers
    toDigits,
    fromDigits,
    primeFactors,
    primes,
    factors,

    -- * for Alternative
    whenA,
    ensure,

    -- * for lists
    chop,
    allSame,
    diff,
    lastn,
    removeSharedPrefix,
    odds,
    evens,
    sequentialGroupBy,
    combinations,
    permutations,
    partitions,
    foci,
    middle,

    -- * for tuples
    mapTuple,

    -- * for boolean
    implies,

    -- * Taps
    tap,
    tapM,
  )
where

import Control.Applicative (Alternative (empty))
import Control.Arrow ((***))
import Control.Monad (guard, join)
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.List (group, inits, unfoldr)
import Data.Tuple (swap)
import Unsafe.Coerce (unsafeCoerce)

-- | Y combinator.
--
-- >>> :{
--  let
--    fibF :: (Int -> Int) -> Int -> Int
--    fibF _ 0 = 0
--    fibF _ 1 = 1
--    fibF f n = f (n - 1) + f (n - 2)
--  in
--    y fibF 10
-- :}
-- 55
y :: (a -> a) -> a
y f = (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))

-- | Combine an unary function and a binary function as @\\x y -> f (g x y) == owl f g@ .
owl :: (c -> d) -> (a -> b -> c) -> a -> b -> d
owl = (.) . (.)

-- | Get digits of an integer.
--
-- >>> toDigits 10 1234
-- [1,2,3,4]
-- >>> toDigits 2 42
-- [1,0,1,0,1,0]
toDigits :: forall a. (Integral a) => a -> a -> [a]
toDigits base = reverse . unfoldr step
  where
    step :: (Alternative f) => a -> f (a, a)
    step n = swap (divMod n base) <$ guard (n /= 0)

-- | Convert digits to an integer.
--
-- >>> fromDigits 10 [1, 2, 3, 4]
-- 1234
-- >>> fromDigits 2 [1, 0, 1, 0, 1, 0]
-- 42
fromDigits :: (Integral a) => a -> [a] -> a
fromDigits base = foldl ((+) . (base *)) 0

-- | Capsulate value in `Alternative`.
whenA :: (Alternative f) => a -> Bool -> f a
whenA = bool empty . pure

-- | `whenA` with a predicate.
ensure :: (Alternative m) => (a -> Bool) -> a -> m a
ensure = (whenA <*>)

-- | Chop a list to lists of every @n@ elements.
chop :: Int -> [a] -> [[a]]
chop n = unfoldr $ fmap (splitAt n) . ensure (not . null)

-- | Return @True@ if the list has all the same elements.
allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (x ==) xs

-- | Compute implication.
implies :: Bool -> Bool -> Bool
implies True False = False
implies _ _ = True

-- | Get rear part of the longer list.
diff :: [a] -> [a] -> [a]
diff [] ys = ys
diff xs [] = xs
diff (_ : xs) (_ : ys) = diff xs ys

-- | Get last @n@ elements of the list.
lastn :: Int -> [a] -> [a]
-- lastn n xs = diff xs $ drop n xs
lastn = (diff <*>) . drop

-- | Apply f on both tuple elements.
--
-- >>> mapTuple (+ 1) (3, 4)
-- (4,5)
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

-- | Remove shared prefix from two lists.
removeSharedPrefix :: (Eq a) => [a] -> [a] -> ([a], [a])
removeSharedPrefix = (unzip .) . (dropWhile (uncurry (==)) .) . zip

-- | Return elements in odd positions.
--
-- >>> odds [1..10]
-- [1,3,5,7,9]
odds :: [a] -> [a]
odds [] = []
odds (x : xs) = x : evens xs

-- | Return elements in even positions.
--
-- >>> evens [1..10]
-- [2,4,6,8,10]
evens :: [a] -> [a]
evens [] = []
evens (_ : xs) = odds xs

-- | Group consequent series.
--
-- >>> sequentialGroupBy (\x y -> succ x == y) [2,3,5,7,11,13,17,18,19]
-- [[2,3],[5],[7],[11],[13],[17,18,19]]
sequentialGroupBy :: forall a. (a -> a -> Bool) -> [a] -> [[a]]
sequentialGroupBy p = foldr f []
  where
    f :: a -> [[a]] -> [[a]]
    f x (ys@(y' : _) : yss) | p x y' = (x : ys) : yss
    f x yss = [x] : yss

-- | List combinations of provided elements
--
-- >>> combinations 2 [1..4]
-- [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
-- >>> combinations 3 [1,2,3,4]
-- [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

-- | List permutations of provided elements
--
-- >>> permutations 2 [1..3]
-- [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = [[]]
permutations n xs = select xs >>= \(y', ys) -> map (y' :) (permutations (n - 1) ys)

select :: [a] -> [(a, [a])]
select [] = error "select: empty list"
select [x] = [(x, [])]
select (x : xs) = (x, xs) : map (second (x :)) (select xs)

-- | Partition of set by k
--
-- >>> partitions 2 [1, 2]
-- [[[1,2],[]],[[2],[1]],[[1],[2]],[[],[1,2]]]
-- >>> length (partitions 3 [1..4]) == 3 ^ 4
-- True
partitions :: Int -> [a] -> [[[a]]]
partitions k = foldr f [replicate k []]
  where
    f :: a -> [[[a]]] -> [[[a]]]
    f x = concatMap $ map (\(xss1, xs, xss2) -> xss1 ++ [x : xs] ++ xss2) . foci

-- | Focus on each element
--
-- >>> foci [1..3]
-- [([],1,[2,3]),([1],2,[3]),([1,2],3,[])]
foci :: [a] -> [([a], a, [a])]
foci [] = []
foci (x : xs) = ([], x, xs) : map (\(ys1, y', ys2) -> (x : ys1, y', ys2)) (foci xs)

-- | Middle element of a list
--
-- >>> middle [1..10]
-- 6
middle :: [a] -> a
middle xs = middle' xs xs

middle' :: [a] -> [a] -> a
middle' [] _ = error "middle: empty list"
middle' (m : _) [] = m
middle' (m : _) [_] = m
middle' (_ : ms) (_ : _ : xs) = middle' ms xs

-- | List of prime numbers.
--
-- >>> take 10 primes
-- [2,3,5,7,11,13,17,19,23,29]
primes :: (Integral a) => [a]
primes = 2 : filter (\n -> primeFactors n == [n]) [3, 5 ..]

-- | List prime factors.
--
-- >>> primeFactors 12312
-- [2,2,2,3,3,3,3,19]
primeFactors :: (Integral a) => a -> [a]
primeFactors = primeFactors' primes
  where
    primeFactors' :: (Integral a) => [a] -> a -> [a]
    primeFactors' pps@(p : ps) n
      | n < 2 = []
      | n < p ^ (2 :: Int) = [n] -- stop early
      | n `mod` p == 0 = p : primeFactors' pps (n `div` p)
      | otherwise = primeFactors' ps n
    primeFactors' _ _ = error "`primes` is a infinite list"

-- | List factors of a number.
--
-- >>> Data.List.sort $ factors 1232
-- [1,2,4,7,8,11,14,16,22,28,44,56,77,88,112,154,176,308,616,1232]
factors :: (Integral a) => a -> [a]
factors = map product . mapM (map product . inits) . group . primeFactors

-- | Execute side effect and return original value.
--
-- >>> "abc" `tap` putStrLn
-- abc
-- "abc"
tap :: (Functor f) => a -> (a -> f b) -> f a
tap x k = x <$ k x

-- | Monad version of 'tap'.
--
-- >>> [1, 2] `tapM` \x -> [(), ()]
-- [1,1,2,2]
tapM :: (Monad m) => m a -> (a -> m b) -> m a
tapM m k = m >>= (`tap` k)
