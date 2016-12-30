-- @Author: krocki
-- @Date:   2016-12-29 16:17:25
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-29 20:31:48

-- Number spiral diagonals
-- 
-- Problem 28
-- 
-- Starting with the number 1 and moving
-- to the right in a clockwise direction
-- a 5 by 5 spiral is formed as follows:

-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13

-- It can be verified that the sum of the
-- numbers on the diagonals is 101.

-- What is the sum of the numbers on the
-- diagonals in a 1001 by 1001 spiral formed in the same way?

-- idea: need to sum a sequence whose elements
-- are separated by increasing intervals
-- [1, (3,5,7,9), (13,17,21,25), ...] until interval is less than 1001

-- take 4 elements separated by interval
-- make an 'arm' of the 'spriral'
take4elems :: (Num a, Enum a) => a -> a -> [a]
take4elems x interval = take 4 [x+interval, x+2*interval..]

sums :: (Ord t, Num t, Enum t) => t -> t -> t
sums x interval
    | interval < 1001 = sum a + sums (last a) (interval+2)
    | otherwise = 0
    where a = take4elems x interval

main = do
    print solution
        -- 1 + 4 elements (interval 2) + ...
        where solution = 1 + (sums 1 2)