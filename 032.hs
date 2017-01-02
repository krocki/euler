-- @Author: krocki
-- @Date:   2016-12-30 13:35:30
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-01 17:43:09

-- Pandigital products
-- Problem 32 
-- 
-- We shall say that an n-digit number is
-- pandigital if it makes use of all the digits 1 to n exactly once; for
-- example, the 5-digit number, 15234, is 1 through 5 pandigital.

-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
-- multiplicand, multiplier, and product is 1 through 9 pandigital.

-- Find the sum of all products whose multiplicand/multiplier/product identity
-- can be written as a 1 through 9 pandigital.

-- HINT: Some products can be obtained in more than one way so be sure to only
-- include it once in your sum.

import Data.List 
import Data.List.Split

-- TODO: optimize

digits = ['1'..'9']

-- generate all permutations of xs
allpermutations :: Eq t => [t] -> [[t]]
allpermutations [] = [[]]
allpermutations xs = [x:rest | x <- xs, rest <- allpermutations (delete x xs)]

-- split list at 2 points, a and b
splitAt2 :: Int -> Int -> [a] -> ([a], [a], [a])
splitAt2 a b xs = (fst z, snd z, snd y)
                  where y = splitAt b xs
                        z = splitAt a (fst y)

-- make all possible pandigital expressions
makeExpr :: [[a]] -> [([a], [a], [a])]
makeExpr xs = [splitAt2 a b x | x <- xs, a <- [1..(l-2)], b <-[(a+1)..l-1]]
               where l = length digits

--access 3rd tuple element
third (_, _, x) = x

--eval an expression, write 0 to fst element if True
eval' :: (String, String, String) -> (Int, Int)
eval' (x,y,z) = ((read x) * (read y) - (read z) :: Int, read z :: Int)

main = do
    let all = (allpermutations digits)
        solution = sum (map (snd) 
                   (nub (filter (\x -> (fst x) == 0) 
                   (map (eval') (makeExpr all)))))
    print solution
