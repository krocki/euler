-- @Author: krocki
-- @Date:   2017-01-16 16:29:39
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-16 16:42:19

{-
Champernowne's constant
Problem 40 

An irrational
decimal fraction is created by concatenating the
positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the
fractional part is 1.

If dn represents the nth digit of the fractional
part, find the value of the following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 ×
d1000000

-}

import Data.Char

create_list :: String
create_list = foldr1 (++) [show x | x <- [0,1..]]

main = do
    let solution = foldr1 (*) [digitToInt (create_list !! x) | x <- [1,10,100,1000,10000,100000,1000000]]
    print solution
