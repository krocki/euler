-- @Author: krocki
-- @Date:   2016-12-18 20:27:48
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-18 20:53:25

-- Longest Collatz sequence
-- 
-- Problem 14
-- 
-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following
-- sequence:

-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1 It can be seen that this
-- sequence (starting at 13 and finishing at 1) contains 10 terms. Although it
-- has not been proved yet (Collatz Problem), it is thought that all starting
-- numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

-- NOTE: Once the chain starts the terms are allowed to go above one million.

-- TODO: do with unfold

import Data.List

collatzsequence :: Int -> [Int]
collatzsequence n 
        | n > 1 = n : collatzsequence (collatz n) 
        | otherwise = [n]

collatz n
        | even n = n `div` 2
        | otherwise = 3*n + 1

main = do
    
    let sequences = [length (collatzsequence n) | n <- [0..1000000-1]]
        max = maximum sequences
        solution = elemIndex max sequences
    
    print solution