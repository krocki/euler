-- @Author: krocki
-- @Date:   2017-01-16 18:41:41
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-16 21:12:49

-- Powerful digit sum
-- Problem 56

-- A googol (10100) is a massive number: one
-- followed by one-hundred zeros; 100100 is almost
-- unimaginably large: one followed by two-hundred
-- zeros. Despite their size, the sum of the
-- digits in each number is only 1.

-- Considering natural numbers of the form, ab,
-- where a, b < 100, what is the maximum digital
-- sum?
import Data.Char

generate :: Integer -> [Integer]
generate x = iterate (*x) 1

sumofdigits :: Show a => a -> Int
sumofdigits x = sum (map (\y -> digitToInt y) (show x))

generate_all_sums :: [[Int]]
generate_all_sums = [map (sumofdigits) (take n (generate (toInteger n))) | n <- [1..99]]

main = do
    let solution = maximum (maximum (generate_all_sums))
    print solution
