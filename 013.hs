-- @Author: krocki
-- @Date:   2016-12-23 20:16:24
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-24 17:26:34
import Data.Char
import Data.List
-- 'transpose' a list of lists M lists of N elements
-- become N lists of M elements

-- converts int to a list of chars, eg. 324 -> [3,2,4]
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

add' :: (Integral a) => [[a]] -> [a]
add' [] = []
add' (x:[]) = reverse (digits (sum x))
add' (x:(xs:xss)) = digit : add' ((insert remainder xs):xss)
            where total = sum x
                  digit = total `mod` 10
                  remainder = (total - digit) `div` 10

-- adds numbers stored as lists of ints
add :: (Integral a) => [[a]] -> [a]
add digits = reverse (add' (reverse digits))

main = do

    content <- readFile "013.data"
    
    let numbers = lines content
        digits = transpose (map (map (digitToInt)) (numbers))
        solution = take 10 (add digits)

    print solution