-- @Author: krocki
-- @Date:   2016-12-25 14:59:54
-- @Last Modified by:   krocki
-- @Last Modified time: 2016-12-26 16:05:52

-- Counting Sundays  
-- 
-- Problem 19
-- 
-- You are given the following information, but
-- you may prefer to do some research for yourself.

-- 1 Jan 1900 was a Monday. Thirty days has September, April, June and
-- November. All the rest have thirty-one, Saving February alone, Which has
-- twenty-eight, rain or shine. And on leap years, twenty-nine. A leap year
-- occurs on any year evenly divisible by 4, but not on a century unless it is
-- divisible by 400. How many Sundays fell on the first of the month during
-- the twentieth century (1 Jan 1901 to 31 Dec 2000)?

months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
monthsleap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

data Date = Date
    { year      :: Int
    , month     :: Int
    , day       :: Int
    } deriving (Show)

addweek :: Date -> Date
addweek (Date y m d) = (Date newyear newmonth (newday `mod` daysinmonth))
                        where newday = d + 7
                              daysinmonth = if (y `mod` 4 == 0) 
                                            then monthsleap !! m
                                            else months !! m
                              newmonth' = m + newday `div` daysinmonth
                              newyear = y + newmonth' `div` 12
                              newmonth = newmonth' `mod` 12

listdates :: Date -> [Date]
listdates (Date y m d) = (Date y m d) : rest
                         where rest = if y < 2001 
                                then (listdates (addweek (Date y m d))) 
                                else []

main = do

    -- First Sunday is 1901/1/6
    -- Set this a starting date, then add 7 days until 2000/31/12 and check
    -- which of those are 1st day of month

    let startingday = Date 1901 0 5
        solution = length (filter (\(Date y m d) -> d == 0) (listdates startingday))

    print solution
