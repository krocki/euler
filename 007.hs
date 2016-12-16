-- @Author: kmrocki
-- @Date:   2016-12-14 19:24:41
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-16 13:39:09

-- 10001st prime  
--
-- Problem 7  By listing the first six prime numbers: 2, 3, 5,
-- 7, 11, and 13, we can see that the 6th prime is 13.

-- What is the 10001st prime number?

-- TODO: find a more elegant (and faster; this one takes ~40s) solution

isPrime :: Integer -> [Integer] -> Bool
isPrime x previousPrimes = and [x `mod` y /=  0 |  y <- previousPrimes]

addPrime :: [Integer] -> Integer -> [Integer]
addPrime previousPrimes x
    | isPrime x previousPrimes = previousPrimes ++ [x]
    | otherwise = previousPrimes

listPrimes :: [Integer] -> Integer -> [Integer]
listPrimes list number
    | length list < 10001 = listPrimes (addPrime list number) (number + 1)
    | otherwise = list

main = do 
    let solution = last (listPrimes [] 2)
    print solution
