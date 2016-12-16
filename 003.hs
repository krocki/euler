-- @Author: kmrocki
-- @Date:   2016-12-15 12:47:00
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-16 13:39:29

-- Largest prime factor
--
-- Problem 3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

-- this will just create an infinite list of factors starting from 2
-- we already know that 1 is a factor

findFactors :: Integer -> [Integer]
findFactors num = [x | x <- [2..], num `mod` x == 0]

-- this will stop when the largest factor is greater
-- than the number divided by that factor, 
-- because there can be no other prime
-- factor above that, just conbination of existing factors

listPrimeCandidateFactors :: Integer -> [Integer]
listPrimeCandidateFactors num = 
     takeWhile (\x -> x < num `div` x) (findFactors num)

-- prime numbers have no factors except 1 and themselves,
-- so we first construct a list of possible factors and check the length

isPrime :: Integer -> Bool
isPrime num = length (listPrimeCandidateFactors num) == 0

-- 1. we list all possible prime factors of 600851475143
-- 2. we filter them and get those which really are primes
-- 3. we take the last one (maximum)

main = do 
    let solution = last [x | x <- listPrimeCandidateFactors 600851475143, isPrime x]
    print solution