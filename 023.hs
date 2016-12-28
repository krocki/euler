-- @Author: krocki
-- @Date:   2016-12-26 21:00:30
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-28 12:04:25

-- Non-abundant sums 
-- Problem 23 

-- A perfect number is a number for which the sum
-- of its proper divisors is exactly equal to the number. For example, the sum
-- of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means
-- that 28 is a perfect number.

-- A number n is called deficient if the sum of its proper divisors is less
-- than n and it is called abundant if this sum exceeds n.

-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
-- number that can be written as the sum of two abundant numbers is 24. By
-- mathematical analysis, it can be shown that all integers greater than 28123
-- can be written as the sum of two abundant numbers. However, this upper
-- limit cannot be reduced any further by analysis even though it is known
-- that the greatest number that cannot be expressed as the sum of two
-- abundant numbers is less than this limit.

-- Find the sum of all the positive integers which cannot be written as the
-- sum of two abundant numbers.

limit = 28123

-- sum of divisors
d :: Int -> Int
d n = sum [x + (n `div` x) | x <- sqrtlimit, n `mod` x == 0] - n - issquare
              where sqrtlimit = takeWhile (\k -> k*k <= n) [1..]
                    sqrt = length (sqrtlimit)
                    issquare = if (sqrt^2 == n) then sqrt else 0


-- is x abundant?
isAbundant :: Int -> Bool
isAbundant x = (d x) > x

-- is x a sum of 2 abundants?
isSumOfAbundants :: Int -> Bool
isSumOfAbundants x = any (\k -> isAbundant (x - k)) (takeWhile (< x) abundants)
                        where abundants = filter (isAbundant) [1..limit]

main = do
    let solution = sum (filter (not . isSumOfAbundants) [1..limit])
    print solution