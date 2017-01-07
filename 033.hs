-- @Author: krocki
-- @Date:   2016-12-30 13:36:59
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-06 21:45:27

-- Digit cancelling fractions
-- Problem 33
-- 
-- The fraction 49/98 is a curious
-- fraction, as an inexperienced mathematician in attempting to simplify it
-- may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by
-- cancelling the 9s.

-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

-- There are exactly four non-trivial examples of this type of fraction, less
-- than one in value, and containing two digits in the numerator and
-- denominator.

-- If the product of these four fractions is given in its lowest common terms,
-- find the value of the denominator.

import Data.List

-- overlap in # of symbols
overlap :: (Eq a) => [a] -> [a] -> Int
overlap [] _ = 0
overlap (x:xs) ys 
    | x `elem` ys = 1 + overlap xs ys
    | otherwise = overlap xs ys

-- initial list of fractions 
-- (less than one in value, and containing two digits in the numerator and
-- denominator.)

fractions :: [(String, String)]
fractions = [(show n, show d) | n <- [10..99], d <- [(n+1)..99]]

-- fractions with at least one shared symbol (simplify-able)
fractions' :: [(String, String)]
fractions' = filter (\(x,y) -> overlap x y > 0) fractions

-- remove shared symbol(s)
-- removeoverlaps ("abc", "cde") == [("ab","de")]
removeoverlaps :: (String, String) -> [(String, String)]
removeoverlaps (a, b) = [(delete x a, delete y b) | x <- a, y <- b, x == y, x /= '0']

-- simplify-able and valid fractions
fractions'' :: [[(String, String)]]
fractions'' = filter (\x -> length x > 0) (map (\(x,y) -> [(x,y) | a <- removeoverlaps (x, y), isDivisibleFraction (x,y) a]) fractions')

-- check is fraction n0/d0 can be simplified using another fraction n1/d1
isDivisibleFraction :: (String, String) -> (String, String) -> Bool
isDivisibleFraction (n0,d0) (n1,d1) = if (d0' > 0) && (n0' > 0) && (d1' > 0) && (n1' > 0) && (n0'/n1') == (d0'/d1') then True else False
                where n0' = read n0 :: Float
                      n1' = read n1 :: Float
                      d0' = read d0 :: Float
                      d1' = read d1 :: Float

main = do
    let r x = read x :: Int
        product = foldl (\(a,b) (x,y) -> (a * (r x), b * (r y))) (1, 1) (map (head) fractions'')
        solution = div (snd product) (fst product)
    print solution