-- @Author: krocki
-- @Date:   2016-12-24 17:30:35
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-24 18:10:30

-- Lattice paths 
-- 
-- Problem 15 
-- 
-- Starting in the top left corner of a 2×2 grid, and
-- only being able to move to the right and down, 
-- there are exactly 6 routes to the bottom right corner.
-- 
-- How many such routes are there through a 20×20 grid?

-- the route length is 40
-- we need to go 20 times down and 20 times right
-- choose 20 locations of D or R out of 40, so 40 choose 20

n_choose_k n k = product [1..n] `div` (product [1..k] * product [1..n-k])

main = do

    let solution = n_choose_k 40 20
    
    print solution




