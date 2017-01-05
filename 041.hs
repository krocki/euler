-- @Author: krocki
-- @Date:   2017-01-04 18:27:04
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-05 09:31:59
import Data.List

digits = ["1","2","3","4","5","6","7","8","9"]

-- for checking primality
findFactors :: Int -> [Int]
findFactors num = [x | x <- [2..], num `mod` x == 0]

listPrimeCandidateFactors :: Int -> [Int]
listPrimeCandidateFactors num = 
    takeWhile (\x -> x < num `div` x) (findFactors num)

isPrime :: Int -> Bool
isPrime num = length (listPrimeCandidateFactors num) == 0

-- make a list of pandigital numbers of length n
pandigital' :: Int -> [Int]
pandigital' n = map (read . concat) (permutations (take n digits))

-- make a list of all possible pandigitial numbers of length 2..9
pandigital :: [Int]
pandigital = [xs | x <- [2..9], xs <- pandigital' x]

-- helper f for filtering
-- sum of digits
sumd :: Integral t => t -> t
sumd 0 = 0
sumd x = (x `mod` 10) + sumd (x `div` 10)

lastdigit :: Integral a => a -> a
lastdigit x = (x `mod` 10)

-- make a list of all possible panditial numbers
-- filter out even, ending with 5 and whose sum of digits if divisible by 3
-- then sort, check for primality in descending order, take 1st one which is OK    
main = do
    let pan = pandigital
        pans = reverse . sort $ (filter (\x -> odd x && (sumd x `mod` 3 /= 0) && (lastdigit x /= 5)) $ reverse pan)
        solution = take 1 (filter (\x -> isPrime x == True) pans)
    print solution