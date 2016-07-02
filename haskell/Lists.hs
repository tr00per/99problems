-- Solutions to "99 Prolog Problems" (c) 2016 Artur Czajka. 2-clause BSD license applies.

-- ScopedTypeVariables is needed to explicity write type signature for `draw` in `problem23`
{-# LANGUAGE ScopedTypeVariables #-}
module Lists where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.List
import           System.Random

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
    where removeNth = uncurry (++) . second (problem16 n . tailOrNull) . splitAt (n-1)
          tailOrNull as = if null as then as else tail as

problem17 :: Int -> [a] -> ([a], [a])
problem17 = splitAt

problem18 :: Int -> Int -> [a] -> [a]
problem18 from to = take (to-from+1) . drop from

problem19 :: Int -> [a] -> [a]
problem19 n = uncurry (flip (++)) . splitAt n

problem20 :: Int -> [a] -> (a, [a])
problem20 n xs = let (as,bs) = splitAt n xs
                 in (head bs, as ++ tail bs)

problem21 :: Int -> a -> [a] -> [a]
problem21 0 a xs = a:xs
problem21 _ a [] = [a]
problem21 n a (x:xs) = x : problem21 (n-1) a xs

problem22 :: Int -> Int -> [Int]
problem22 = enumFromTo

problem23 :: forall a . Int -> [a] -> State StdGen [a]
problem23 n collection = mapM (draw $ length collection) [1..n]
    where draw :: Int -> Int -> State StdGen a
          draw limit _ = do
              (x, newGen) <- next <$> get
              put newGen
              let idx = x `mod` limit
              return (collection !! idx)

runProblem23 :: Int -> [a] -> IO [a]
runProblem23 n collection = evalState (problem23 n collection) <$> newStdGen

problem24 :: Int -> Int -> StateT [Int] (State StdGen) ()
problem24 n limit
    | n >= limit = iter
    | otherwise = error ("Not enough elements to pick " ++ show n ++ " unique values")
    where
        iter :: StateT [Int] (State StdGen) ()
        iter = do
            result <- get
            r <- lift randomize
            let newResult = if r `notElem` result
                then r:result
                else result
            put newResult
            unless (length newResult == n) iter

        randomize :: State StdGen Int
        randomize = do
            (x, newGen) <- next <$> get
            put newGen
            return (x `mod` limit + 1)

runProblem24 :: Int -> Int -> IO [Int]
runProblem24 n limit = evalState (execStateT (problem24 n limit) []) <$> newStdGen

problem25 :: [a] -> StdGen -> [a]
problem25 xs gen = perms !! idx
    where
        perms = permutations xs
        (idx, _) = first (`mod` length perms) $ next gen

runProblem25 :: [a] -> IO [a]
runProblem25 xs = problem25 xs <$> newStdGen

problem26 :: Int -> [a] -> [[a]]
problem26 = iter
    where
        iter 0 _ = []
        iter n xs
            | n > xslen = []
            | otherwise = do
            spl <- [1..xslen-1]
            let (as, bs) = splitAt spl xs
                lastas = last as
            bs' <- iter (n-1) bs
            return (lastas : bs')
            where
                xslen = length xs
