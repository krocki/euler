-- @Author: krocki
-- @Date:   2017-01-05 09:40:11
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-05 13:23:31

{-
Self powers
Problem 48
The series, 11 + 22 + 33 + ... + 1010 = 10405071317.

Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.
-}

-- convert an int to a list of digits
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- mult :: Integral x => [x] -> [x] -> [x]
-- mult xs ys = mult' (reverse xs) (reverse ys)
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

-- list-single digit mult
bigpow :: Int -> Int -> [Int]
bigpow x 1 = digits x
bigpow x n = mult'' x (bigpow x (n-1))

mult'' :: Int -> [Int] -> [Int]
mult'' x y = reverse (mult' x (reverse y) 0)

mult' :: Int -> [Int] -> Int -> [Int]
mult' x [] c = if c == 0 then [] else reverse (digits c) ++ []
mult' x (y:ys) carry = d : (mult' x ys next_carry)
                    where r = (x * y) + carry
                          d = mod r 10
                          next_carry = div r 10

main = do
    let n = 1000
    let numbers = (foldl (add) [] ([(bigpow x x) | x <- [1..n]]))
        ten = concat . map (show) $ drop (length numbers - 10) numbers
    print ten