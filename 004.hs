-- @Author: kmrocki
-- @Date:   2016-12-15 14:42:03
-- @Last Modified by:   kmrocki
-- @Last Modified time: 2016-12-15 14:54:31

-- Largest palindrome product 
-- 
-- Problem 4 
-- 
-- A palindromic number reads the same
-- both ways. The largest palindrome made from the product of two 2-digit
-- numbers is 9009 = 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

isPalindrome :: [Char] -> Bool
isPalindrome sequence = sequence == reverse sequence

palindromes :: [Integer]
palindromes = [x * y | x <- [1..999], y <- [1..999], isPalindrome . show $ x * y]

main = do 
    let solution = maximum palindromes
    print solution