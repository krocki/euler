-- @Author: krocki
-- @Date:   2016-12-26 21:01:26
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-26 21:12:07

-- Lexicographic permutations
-- 
-- Problem 24
-- 
-- A permutation is an ordered arrangement of objects.
-- For example, 3124 is one possible permutation
-- of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

-- 012   021   102   120   201   210

-- What is the millionth lexicographic
-- permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

import Data.List

-- generate all permutations 
permutations' :: (Ord a) => [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [x:rest | x <- xs, rest <- permutations' (delete x xs)]

-- sort and return element no 1000000 == [2,7,8,3,9,1,5,4,6,0]
main = do
    let ordered = sort (permutations [0,1,2,3,4,5,6,7,8,9])
        solution = ordered !! 999999
    print solution