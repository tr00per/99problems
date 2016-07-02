-- Solutions to "99 Prolog Problems" (c) 2016 Artur Czajka. 2-clause BSD license applies.

module Arithmetic where

import           Control.Arrow
import           Data.List
import           Data.Numbers.Primes

problem01 :: Integral a => a -> Bool
problem01 = isPrime

problem02 :: Integral a => a -> [a]
problem02 = primeFactors

problem03 :: Integral a => a -> [(a, Int)]
problem03 =  map (head &&& length) . group . primeFactors

problem04 :: (Integral a, Ord a) => a -> a -> [a]
problem04 min max = takeWhile (<max) $ dropWhile (<min) primes

problem05 :: Int -> (Int, Int)
problem05 target = head $ filter (\(x,y) -> x+y == target) pairs
    where
        pairs :: [(Int, Int)]
        pairs = undefined
        somePrimes = takeWhile (<target) primes
