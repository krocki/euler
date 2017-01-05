-- @Author: krocki
-- @Date:   2016-12-28 12:07:38
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-04 18:24:02

-- Reciprocal cycles
-- 
-- Problem 26
-- 
-- A unit fraction contains 1 in the numerator.
-- The decimal representation of the unit fractions
-- with denominators 2 to 10 are given:

-- 1/2 =   0.5
-- 1/3 =   0.(3)
-- 1/4 =   0.25
-- 1/5 =   0.2
-- 1/6 =   0.1(6)
-- 1/7 =   0.(142857)
-- 1/8 =   0.125
-- 1/9 =   0.(1)
-- 1/10 =   0.1

-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle.
-- It can be seen that 1/7 has a 6-digit recurring cycle.

-- Find the value of d < 1000 for which 1/d contains the longest recurring
-- cycle in its decimal fraction part.

-- divide implements division a bit obfuscated, 
-- 1st elem of tuple acc has digits
-- acc is used to detect cycles, 
-- remove 2nd elem to get divide without loop break on cycles

-- standard:
-- divide num denominator
--     | num < denominator = [0] ++ divide (num*10) denominator
--     | mod num denominator /= 0 = [whole] ++ divide rest denominator
--     | otherwise = [whole]
--     where  whole = div num denominator
--            rest = (10 * (num - whole * denominator))

import Data.List (elemIndex, foldl1')

divide :: Integral a => a -> a -> [(a, a)] -> [(a, a)]
divide num denominator acc
    | (num,denominator) `elem` acc = acc ++ [(num,denominator)]
    | num < denominator =  divide (num*10) denominator (acc ++ [(num,denominator)])
    | mod num denominator /= 0 = divide rest denominator (acc ++ [(num,denominator)])
    | otherwise = acc ++ [(num,denominator)]
    where  whole = div num denominator
           rest = (10 * (num - whole * denominator))

-- cycle length calculated by taking the index of
-- the last element of acc, finding a mathing element
-- and returning its index, then the distance between these
-- two is the cycle length

cycleLength x = l - length (takeWhile (/= a !! (l - 1)) a) - 1
                 where a = divide 1 x []
                       l = length a

argmax :: Ord b => (a -> b) -> [a] -> a
argmax f = foldl1' (\x y -> if (f y) > (f x) then y else x)

main = do
    let solution = argmax cycleLength [1..1000]
    print solution
