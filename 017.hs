-- @Author: krocki
-- @Date:   2016-12-24 19:43:37
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-24 20:26:32

-- Number letter counts
-- 
-- Problem 17
-- 
-- If the numbers 1 to 5 are written out in words: 
-- one, two, three, four, five, then there are 
-- 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- 
-- If all the numbers from 1 to 1000 (one thousand) 
-- inclusive were written out in words, how many 
-- letters would be used?
-- 
-- NOTE: Do not count spaces or hyphens. 
-- For example, 342 (three hundred and forty-two) 
-- contains 23 letters and 115 (one hundred and fifteen)
-- contains 20 letters. The use of "and" when
-- writing out numbers is in compliance with British usage.

twenty = [ "", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty"]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

numberToString :: Int -> String
numberToString x
               | x <= 20 = twenty !! x
               | x < 100 = tens !! (x `div` 10) ++ " " ++ twenty !! (x `mod` 10)
               | x == 1000 = "one thousand"
               | x `mod` 100 == 0 = twenty !! (x `div` 100) ++ " hundred"
               | x <= 999 = twenty !! (x `div` 100) ++ " hundred and " ++ numberToString (x `mod` 100)
               | otherwise = "x > 1000"

main = do
    let numbers = map (numberToString) [1..1000]
        characters = map (length . filter (\x -> x `elem` ['a'..'z'])) numbers
    print (sum characters)

