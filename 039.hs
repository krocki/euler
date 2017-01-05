-- @Author: krocki
-- @Date:   2017-01-04 16:19:00
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-04 18:13:40

-- Integer right triangles 
-- Problem 39 
-- If p is the perimeter of a right angle
-- triangle with integral length sides, {a,b,c}, there are exactly three
-- solutions for p = 120.

-- {20,48,52}, {24,45,51}, {30,40,50}

-- For which value of p ≤ 1000, is the number of solutions maximised?
-- idx, val
import Data.List (elemIndex, foldl1')

argmax :: Ord b => (a -> b) -> [a] -> a
argmax f = foldl1' (\x y -> if (f y) > (f x) then y else x)

solutions :: Int -> [(Int, Int, Int)]
solutions p = [(a,b,c) | a <- [1..p-3], b <- [(a+1)..p-a-1], c <- [p - a - b], (c^2 - a^2 - b^2) == 0]

main = do
    let solution = argmax (length . solutions) $ [1..1000]
    print solution