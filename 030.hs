-- @Author: krocki
-- @Date:   2016-12-29 16:23:04
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-30 18:49:11

-- Digit fifth powers
-- 
-- Problem 30
-- 
-- Surprisingly there are only three numbers
-- that can be written as the sum of fourth powers of their digits:

-- 1634 = 14 + 64 + 34 + 44 8208 = 84 + 24 + 04 + 84 9474 = 94 + 44 + 74 + 44
-- As 1 = 14 is not a sum it is not included.

-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.

-- Find the sum of all the numbers that can be written as the sum of fifth
-- powers of their digits.

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- it is OK to consider only numbers up to 'limit', 
-- since all above are greater than the the max sum of powers

limit :: Integer
limit = head (dropWhile (==0) (zipWith (\x y ->
        if x > y then 0 else x) maxsum minnumber))
        where maxsum = map (9^5*) [1..]
              minnumber = map (10^) [1..]

powers :: Integer
powers = sum [x | x <- [2..limit], digitspowerssum x == x]
            where digitspowerssum x = sum (map (^5) (digits x))

main = do
    print solution
        where solution = powers