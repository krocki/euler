-- @Author: krocki
-- @Date:   2017-01-16 18:37:23
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-17 17:18:38


-- Coded triangle numbers
-- Problem 42

-- The nth term of the sequence of triangle
-- numbers is given by, tn = ½n(n+1); so the first
-- ten triangle numbers are:

-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

-- By converting each letter in a word to a number
-- corresponding to its alphabetical position and
-- adding these values we form a word value. For
-- example, the word value for SKY is 19 + 11 + 25
-- = 55 = t10. If the word value is a triangle
-- number then we shall call the word a triangle
-- word.

-- Using words.txt (right click and 'Save
-- Link/Target As...'), a 16K text file containing
-- nearly two-thousand common English words, how
-- many are triangle words?
import Data.Char
import Data.List
import Data.List.Split 

word2val :: [Char] -> Int
word2val word = sum (map (\x -> subtract 64 (ord x)) word)

triangle_numbers = take 1000 (map (\x -> (x * (x+1)) `div` 2) [1..])

main = do
    content <- readFile "p042_words.txt"
    let words = splitOn "," content
        filtered = map (\x -> filter (`elem` ['A'..'Z']) x) words
        num_triangle = length (filter (`elem` triangle_numbers) (map (word2val) filtered))
        solution = num_triangle
    print solution
