-- @Author: kmrocki
-- @Date:   2016-12-16 10:53:03
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-16 13:37:08

-- Summation of primes
-- 
-- Problem 10
-- 
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.

-- Sieve of Eratosthenes was as the name implies invented by Eratosthenes who
-- was a Greek Mathematician living around 200 BC.

-- The algorithm needs to have an upper limit for the primes to find. Lets
-- call this limit N

-- The algorithm works as follows.

-- Create a list l of consecutive integers {2,3,…,N}. Select p as the first
-- prime number in the list, p=2. Remove all multiples of p from the l. set p
-- equal to the next integer in l which has not been removed. Repeat steps 3
-- and 4 until p2 > N, all the remaining numbers in the list are primes

-- TODO: faster implementation - this needs ~ 68 s

-- sieve: takes 1st element of the list and removes its multiples from the list

sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (x:xs) 
     | x*x < last xs = x:sieve (removeMultiples x xs)
     | otherwise = x:xs

-- removeMultiples: removes multiples of n from the list

removeMultiples :: (Integral a) => a -> [a] -> [a]
removeMultiples n [] = []
removeMultiples n (x:xs)
     | x `mod` n == 0 = removeMultiples n xs
     | otherwise = x:removeMultiples n xs

main = do 
    let solution = sum (sieve [2..2000000-1])
    print solution
