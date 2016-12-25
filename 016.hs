-- @Author: krocki
-- @Date:   2016-12-24 18:13:21
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-24 19:43:30

-- Power digit sum
-- 
-- Problem 16 
-- 
-- 2^15 = 32768 and the sum of its digits is 
-- 3 + 2 + 7 + 6 + 8 = 26.
-- 
-- What is the sum of the digits of the number 2^1000?

-- [2] -> [4]
-- [4] -> [8]
-- [8] -> [1,6]
-- [1,6] -> [3,2]
-- [3,2] -> [6,4]
-- [6,4] -> [1,2,8]
-- ...

double :: [Int] -> [Int]
double xs = reverse . (carry 0). reverse $ (map (*2) xs)
            where carry 0 [] = []
                  carry c [] = [c]
                  carry c (x:xs) = [digit] ++ carry remainder xs
                    where digit = x `mod` 10 + c
                          remainder = x `div` 10

-- powers' starts with [x] and adds consecutive powers of 2
-- eg. [[1], [2], [4], [8], ...]

powers' :: [Int] -> Int -> [[Int]]
powers' x limit
    | limit > 0 = [x] ++ powers' (double x) (limit-1)
    | otherwise = [x]

-- last (powers' [1] 1000) is 2^1000

main = do
    let solution = powers' [1] 1000
    print (sum (last solution))
