-- @Author: kmrocki
-- @Date:   2016-12-15 15:01:51
-- @Last Modified by:   kmrocki
-- @Last Modified time: 2016-12-15 15:34:09

-- Smallest multiple
-- Problem 5

-- 2520 is the smallest number that can be divided by each of the numbers from
-- 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?

-- check if number is divisible by all elements in list

isDivisible :: Integer -> [Integer] -> Bool
isDivisible number list = and [number `mod` x == 0 | x <- list]

-- check every 2520 since we know that a number divisible by 1,2,3...20 
-- has to be divisible by 1,2,3...10
-- since the lcm of 1..10 is 2520, we can consider only multiples of 2520
-- further optimizations are possible if lcm of 1..11, 1.12, etc are considered only

smallestDivisibleNumber :: [Integer] -> Integer
smallestDivisibleNumber list = head (dropWhile (\x -> isDivisible x list == False) [2520,5040..])

main = do 
    let solution = smallestDivisibleNumber [1..20]
    print solution