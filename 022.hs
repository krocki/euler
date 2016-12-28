-- @Author: krocki
-- @Date:   2016-12-26 20:59:43
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-27 20:22:30

-- Names scores
-- Problem 22

-- Using names.txt (right click and 'Save Link/Target As...'),
-- a 46K text file containing over five-thousand first names,
-- begin by sorting it into alphabetical order. 
-- Then working out the alphabetical value for each name, 
-- multiply this value by its alphabetical position
-- in the list to obtain a name score.

-- For example, when the list is sorted into alphabetical order,
-- COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53,
-- is the 938th name in the list. So,
-- COLIN would obtain a score of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?

import Data.List
import Data.Char
import Data.List.Split 

name2score :: [Char] -> Int
name2score name = sum (map (\x -> subtract 64 (ord x)) name)

main = do

    content <- readFile "p022_names.txt"

    let names = splitOn "," content
        filtered = map (\x -> filter (`elem` ['A'..'Z']) x) names
        sorted = sort filtered
        scored = map (name2score) sorted
        scored_mult_position = [(i+1) * scored !! i | i <- [0..((length scored)-1)]]
        solution = sum (scored_mult_position)

    print solution