module Lists where

import Data.List
import Control.Arrow

problem01 :: [a] -> a
problem01 = last

problem02 :: [a] -> a
problem02 [] = error "Not enough elements in the list"
problem02 [x,_] = x
problem02 (_:xs) = problem02 xs

problem03 :: [a] -> Int -> a
problem03 = (!!)

problem04 :: [a] -> Int
problem04 = length

problem05 :: [a] -> [a]
problem05 = reverse

problem06 :: Eq a => [a] -> [a] -> Bool
problem06 = (==) . reverse

problem07 :: [[a]] -> [a]
problem07 = concat

problem08 :: Eq a => [a] -> [a]
problem08 = map head . group

problem09 :: Eq a => [a] -> [[a]]
problem09 = group

problem10 :: Eq a => [a] -> [(Int, a)]
problem10 = uncurry zip . (map length &&& map head) . group

-- problem11 - List needs to be homogeneous

-- Based on output of problem 10 instead of 11
problem12 :: [(Int, a)] -> [a]
problem12 = concatMap (uncurry replicate)

-- problem13 - List needs to be homogeneous

problem14 :: [a] -> [a]
problem14 = concatMap (replicate 2)

problem15 :: Int -> [a] -> [a]
problem15 n = concatMap (replicate n)

problem16 :: Int -> [a] -> [a]
problem16 _ [] = []
problem16 n xs = removeNth xs
    where removeNth = uncurry (++) . second (problem16 n . safeTail) . splitAt (n-1)
          safeTail as = if null as then as else tail as

problem17 :: Int -> [a] -> ([a], [a])
problem17 = splitAt

problem18 :: Int -> Int -> [a] -> [a]
problem18 from to = take (to-from+1) . drop (from-1)
