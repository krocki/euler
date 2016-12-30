-- @Author: krocki
-- @Date:   2016-12-29 16:22:22
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-29 20:30:38

-- Distinct powers
-- 
-- Problem 29
-- 
-- Consider all integer combinations of ab for 2 ≤
-- a ≤ 5 and 2 ≤ b ≤ 5:

-- 22=4, 23=8, 24=16, 25=32 32=9, 33=27, 34=81, 35=243 42=16, 43=64, 44=256,
-- 45=1024 52=25, 53=125, 54=625, 55=3125 If they are then placed in numerical
-- order, with any repeats removed, we get the following sequence of 15
-- distinct terms:

-- 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

-- How many distinct terms are in the sequence generated by ab for 2 ≤ a ≤ 100
-- and 2 ≤ b ≤ 100?

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
