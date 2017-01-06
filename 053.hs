-- @Author: krocki
-- @Date:   2017-01-05 20:18:22
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-05 20:28:46

{-

Combinatoric selections
Problem 53

There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, 5C3 = 10.

In general,

nCr =    n! r!(n−r)! ,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1. It is
not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are
greater than one-million? -}

main = do
    let solution = sum [1 | n <- [1..100], k <- [1..n], (choose n k) > 10^6]
    print solution

choose :: Integral a => a -> a -> a
choose n k = (product [(n-k+1)..n]) `div` (product [1..k])