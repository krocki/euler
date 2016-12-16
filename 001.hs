-- @Author: kmrocki
-- @Date:   2016-12-14 14:38:50
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-16 13:39:52

-- Multiples of 3 and 5 
-- 
-- Problem 1 
-- 
-- If we list all the natural numbers below 10
-- that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these
-- multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.

sumOfMultiplies :: (Integral a) => [a] -> a
sumOfMultiplies range = sum . filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) $ range

main = do 
    let solution = sumOfMultiplies . takeWhile (<1000) $ [1..]
    print solution
