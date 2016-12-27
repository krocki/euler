-- @Author: krocki
-- @Date:   2016-12-26 21:13:18
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-26 22:32:36

-- 1000-digit Fibonacci number
-- Problem 25
-- 
-- The Fibonacci sequence is defined by the recurrence relation:

-- Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1. Hence the first 12 terms will
-- be:

-- F1 = 1 F2 = 1 F3 = 2 F4 = 3 F5 = 5 F6 = 8 F7 = 13 F8 = 21 F9 = 34 F10 = 55
-- F11 = 89 F12 = 144 The 12th term, F12, is the first term to contain three
-- digits.

-- What is the index of the first term in the Fibonacci sequence to contain
-- 1000 digits?

-- From Problem 20:

-- convert an int to a list of digits
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- pad a number to length n with a symbol 
pad :: [Int] -> Int -> Int -> [Int]
pad seq n symbol = replicate (n - length seq) symbol

-- add 2 numbers with carry
add :: [Int] -> [Int] -> [Int]
add xs ys = reverse (add' (reverse xs') (reverse ys') 0)
            where paddedlength = max (length xs) (length ys)
                  xs' = (pad xs paddedlength 0) ++ xs
                  ys' = (pad ys paddedlength 0) ++ ys


-- helper function for add
add' :: [Int] -> [Int] -> Int -> [Int]
add' [] [] c = digits c ++ []
add' (x:xs) (y:ys) carry = [d] ++ (add' xs ys c)
                        where d' = x + y + carry
                              d = d' `mod` 10
                              c = d' `div` 10

-- fibonacci sequence
fibseq :: [[Int]] -> Int -> [[Int]]
fibseq (a:b:c) x
    | (length a < x) = fibseq((add a b):a:b:c) x
    | otherwise = b:c

-- generate a sequence of fibonacci numbers and figure out which index is the first one to have 1000 digits
-- 4782
main = do
    let solution = length (takeWhile (/= 1000) (reverse (map (\x -> length x) (fibseq [[1],[1]] 1001)))) + 1
    print solution