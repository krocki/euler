-- @Author: krocki
-- @Date:   2017-01-05 14:40:42
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-05 15:02:22

{-
Permuted multiples
Problem 52

It can be seen that the number, 125874, and its double, 251748, contain
exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
contain the same digits. 
-}

import Data.List

-- convert an int to a list of digits
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- check if x is a permutation of y
isPermutation :: (Eq a) => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys
    | length (x:xs) /= length ys = False
    | otherwise = x `elem` ys && isPermutation xs (delete x ys)

main = do
    let smallest = head [x | x <- [1..], isPermutation (digits x) (digits (2*x)) 
                        && isPermutation (digits x) (digits (3*x))
                        && isPermutation (digits x) (digits (4*x))
                        && isPermutation (digits x) (digits (5*x))
                        && isPermutation (digits x) (digits (6*x))]
    print smallest