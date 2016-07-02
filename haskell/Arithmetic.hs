-- Solutions to "99 Prolog Problems" (c) 2016 Artur Czajka. 2-clause BSD license applies.

module Arithmetic where

import           Control.Arrow
import           Control.Monad
import           Data.List
import           Data.Numbers.Primes

problem01 :: Integral a => a -> Bool
problem01 = isPrime

problem02 :: Integral a => a -> [a]
problem02 = primeFactors

problem03 :: Integral a => a -> [(a, Int)]
problem03 =  map (head &&& length) . group . primeFactors

problem04 :: (Integral a, Ord a) => a -> a -> [a]
problem04 low high = takeWhile (<high) $ dropWhile (<low) primes

problem05 :: Int -> (Int, Int)
problem05 target = head $ filter (\(x,y) -> x+y == target) pairs
    where
        pairs :: [(Int, Int)]
        pairs = [(x,y) | x <- somePrimes, y <- somePrimes]
        somePrimes = takeWhile (<target) primes

problem06 :: Int -> Int -> [(Int, Int)]
problem06 low high = let low' = max low 4 in map problem05 [low',low'+2..high]

testProblem06 limit = forM_ (filter bothBigger $ problem06 2 limit) $ \(x,y) ->
    putStrLn $ show (x+y) ++ " = " ++ show x ++ " + " ++ show y
    where
        bothBigger :: (Int, Int) -> Bool
        bothBigger pair = case ((>50) *** (>50)) pair of
            (True, True) -> True
            _            -> False

problem07 :: Int -> Int -> Int
problem07 x y = iter (max x y) (min x y)
    where
        iter a b = case a `quotRem` b of
            (0, _) -> error "No GCD"
            (_, 0) -> b
            (q, r) -> iter b r

problem08 :: Int -> Int -> Bool
problem08 x y = (==1) $ problem07 x y

problem09 :: Int -> Int
problem09 1 = 1
problem09 x = length $ filter (problem08 x) [1..x-1]

problem10 :: Int -> Int
problem10 x = product $ flip map (problem03 x) $ \(p,m) ->
    (p - 1) * p ^ (m-1)

{- Problem 11 in GHCi
> :set +s
> Arithmetic.problem10 10090
4032
(0.01 secs, 5,647,280 bytes)
> Arithmetic.problem09 10090
4032
(0.07 secs, 40,670,544 bytes)
> Arithmetic.problem10 10090
4032
(0.00 secs, 0 bytes)
> Arithmetic.problem09 10090
4032
(0.08 secs, 38,534,040 bytes)
-}
