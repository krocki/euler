-- @Author: kmrocki
-- @Date:   2016-12-15 21:05:57
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-16 13:39:22

-- Special Pythagorean triplet
-- 
-- Problem 9
-- 
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a2 + b2 = c2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- a + b + c = 1000, so c = 1000 - a - b

triplets :: [Integer]
triplets = [a * b * c | a <- [1..1000], b <- [a..1000-a], let c = 1000 - a - b, a^2 + b^2 == c^2]

main = do
    let solution = head triplets
    print solution
