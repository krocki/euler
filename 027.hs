-- @Author: krocki
-- @Date:   2016-12-30 13:34:45
-- @Last Modified by:   krocki
-- @Last Modified time: 2017-01-07 21:46:48

-- Quadratic primes 
-- Problem 27 
-- Euler discovered the remarkable quadratic
-- formula:

-- n2+n+41 n 2 + n + 41 It turns out that the formula will produce 40 primes
-- for the consecutive integer values 0≤n≤39 0 ≤ n ≤ 39 . However, when
-- n=40,402+40+41=40(40+1)+41 n = 40 , 40 2 + 40 + 41 = 40 ( 40 + 1 ) + 41  is
-- divisible by 41, and certainly when n=41,412+41+41 n = 41 , 41 2 + 41 + 41
-- is clearly divisible by 41.

-- The incredible formula n2−79n+1601 n 2 − 79 n + 1601  was discovered, which
-- produces 80 primes for the consecutive values 0≤n≤79 0 ≤ n ≤ 79 . The
-- product of the coefficients, −79 and 1601, is −126479.

-- Considering quadratics of the form:

-- n2+an+b n 2 + a n + b , where |a|<1000 | a | < 1000  and |b|≤1000 | b | ≤
-- 1000


-- where |n| | n |  is the modulus/absolute value of n n

-- e.g. |11|=11 | 11 | = 11  and |−4|=4 | − 4 | = 4 Find the product of the
-- coefficients, a a  and b b , for the quadratic expression that produces the
-- maximum number of primes for consecutive values of n n , starting with n=0
-- n = 0 .

removeMultiples n [] = []
removeMultiples n (x:xs)
     | x `mod` n == 0 = removeMultiples n xs
     | otherwise = x:removeMultiples n xs


sieve (x:xs)
    | x*x < (last xs) = x:sieve (removeMultiples x xs)
    | otherwise = x:xs

compute n a b = n*n + n*a + b

countprimes' :: Int -> Int -> Int -> [Int] -> Int
countprimes' n a b primes
    | e > maximum primes = countprimes' n a b (sieve [2..2*(maximum primes)])
    | (e `elem` primes) == True = 1 + countprimes' (n+1) a b primes
    | otherwise = 0
        where e = compute n a b

countprimes primes = [(a,b,k) | a <- [-999..1000], b <- sieve [2..1000], let k = (countprimes' 0 a b primes), k > 0] 

main = do
    -- recompute primes as needed in countprimes', initially up to 10000
    let primes = sieve [2..10000]
        solution = foldl1 (\(a',b',n') (a,b,n) -> if n > n' then (a,b,n) else (a',b',n')) (countprimes primes)
    print solution