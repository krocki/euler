-- @Author: krocki
-- @Date:   2016-12-30 13:37:31
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-02 08:32:10

-- Circular primes
-- Problem 35
-- The number, 197, is called a circular prime
-- because all rotations of the digits: 197, 971, and 719, are themselves
-- prime.

-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
-- 71, 73, 79, and 97.

-- How many circular primes are there below one million?

-- TODO: optimize

import Data.List

shift :: [a] -> Int -> [a]
shift l n = drop n l  ++ take n l

allRotations :: [a] -> [[a]]
allRotations l = [shift l i | i <- [0 .. (length l) -1]]

removeMultiples :: (Integral a) => a -> [a] -> [a]
removeMultiples n [] = []
removeMultiples n (x:xs)
     | x `mod` n == 0 = removeMultiples n xs
     | otherwise = x:removeMultiples n xs

sieve :: Integral a => [a] -> [a]
sieve (x:xs)
    | x*x < (last xs) = x:sieve (removeMultiples x xs)
    | otherwise = x:xs

main = do 
    let primes = sieve [2..1000000-1]
        solution = length [x | x <- primes, allin (rots x) primes]
            where rots x = allRotations (show x)
                  allin a b =  all (\x -> (read x :: Int) `elem` b) a
    print solution