-- @Author: krocki
-- @Date:   2016-12-29 16:23:49
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-30 13:33:10

-- Coin sums
-- 
-- Problem 31
-- 

-- In England the currency is made up of pound, £, and pence, p, and there are
-- eight coins in general circulation:

-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p). It is possible to make
-- £2 in the following way:

-- 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p How many different ways can £2 be
-- made using any number of coins?

-- solution #1, just sum
-- changeways :: [Int] -> Int -> Int
-- changeways 0 [] = 1
-- changeways _ [] = 0
-- changeways n (c:cs) = sum [changeways (n-i*c) cs | i <- [0..(div n c)]]

-- solution #2, enumerate
changeways :: (Ord t, Num t) => t -> [t] -> [[t]]
changeways 0 _  = [[]]
changeways n [] = []
changeways n coins@(c:cs) 
            | c <= n = map (c:) (changeways (n-c) coins) 
                       ++ changeways n cs
            | otherwise = changeways n cs
main = do 
    let solution = length (changeways 200 [200,100,50,20,10,5,2,1])
    print solution