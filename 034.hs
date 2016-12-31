-- @Author: krocki
-- @Date:   2016-12-29 16:24:34
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-30 19:39:16

-- Digit factorials
-- 
-- Problem 34
-- 
-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- Find the sum of all numbers which are
-- equal to the sum of the factorial of their digits.
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

-- we need just 0 to 9, so make a LUT
factorials :: [Int]
factorials = [1,1,2,6,24,120,720,5040,40320,362880]

-- convert an int to a list of digits
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- sum factorials of digits
sumfactdigits :: Int -> Int
sumfactdigits 0 = 0
sumfactdigits xs = sum (map (\x -> factorials !! x) (digits xs))

-- it is OK to consider only numbers up to 'limit', 
-- since all factorials above are greater than the the sum of digit factorials

main = do

    let limit = head (dropWhile (==0) (zipWith (\x y ->
                if x > y then 0 else x) maxsum minnumber))
                where maxsum = map ((factorials !! 9)*) [1..]
                      minnumber = map (10^) [1..]
        solution = sum [x | x <- [3..limit], sumfactdigits x == x]
    
    print solution
