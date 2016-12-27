-- @Author: krocki
-- @Date:   2016-12-25 15:03:58
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-26 20:56:11

-- Factorial digit sum
-- 
-- Problem 20
-- 
-- n! means n × (n − 1) × ... × 3 × 2 × 1

-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! 
-- is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

-- Find the sum of the digits in the number 100!

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

-- multiply an int by n
mult :: Int -> [Int] -> [Int]
mult 1 xs = xs
mult n xs = add xs (mult (n-1) xs)

-- factorial
fact :: Int -> [Int]
fact 1 = [1]
fact n = mult n (fact (n-1))

main = do
    let solution = sum (fact 100)
    print solution              