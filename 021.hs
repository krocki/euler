-- @Author: krocki
-- @Date:   2016-12-26 20:57:25
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-27 19:46:07

-- Amicable numbers  
--
-- Problem 21  
-- 
-- Let d(n) be defined as the sum of proper
-- divisors of n (numbers less than n which divide evenly into n). If d(a) = b
-- and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a
-- and b are called amicable numbers.

-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
-- 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
-- 71 and 142; so d(284) = 220.

-- Evaluate the sum of all the amicable numbers under 10000.

import Data.List

divisors :: Integral a => a -> [a]
divisors n = sort (foldl1 (++) (divisors' n))

divisors' :: Integral t => t -> [[t]]
divisors' n = [[1]] ++ [[x] ++ [n `div` x] | x <- [2..n], n `mod` x == 0, x*x < n]

d :: Integral a => a -> a
d n = sum (divisors n)

-- TODO: can improve this by changing inner loop to y <- [x..10000] and adding
-- x and d x to the list at the same time
amicablelist :: Integer -> [Integer]
amicablelist limit = [x | x <- [1..limit], y <- [1..limit], (d x == y) && (d y == x) && (x /= y)]

main = do
    print (sum (amicablelist 10000))