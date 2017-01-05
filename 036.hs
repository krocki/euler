-- @Author: krocki
-- @Date:   2017-01-04 16:07:15
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-04 16:17:06

-- Double-base palindromes
-- Problem 36
-- The decimal number, 585 = 10010010012
-- (binary), is palindromic in both bases.

-- Find the sum of all numbers, less than one million, which are palindromic
-- in base 10 and base 2.

-- (Please note that the palindromic number, in either base, may not include
-- (leading zeros.)

palindrome :: Eq a => [a] -> Bool
palindrome x = x == reverse x

int2bin :: Int -> String
int2bin 0 = []
int2bin x
    | x `mod` 2 == 0 = int2bin (x `div` 2) ++ ['0']
    | x `mod` 2 == 1 = int2bin (x-1) ++ ['1']

palindrome2bases :: Int -> Bool
palindrome2bases x = palindrome (show x) && palindrome (int2bin x)

main = do
    let solution = sum (takeWhile (<1000000) [x | x <-[1..], palindrome2bases x == True])
    print solution