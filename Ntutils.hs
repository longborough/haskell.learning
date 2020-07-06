module Ntutils (
       iSqrt, iSqrem, isPrTo, isPrime, firstFalse, firstTrue, primeFactors, primeList
       ) where

-- ===========================================================
-- Two useful lists
-- dlist:   List of increments between possible primes
-- plist:   Running total = list of possible primes (all prime to 2,3,5,7)
-- x is prime => x is a member of plist
-- -----------------------------------------------------------
dlist = 2 : 1 : 2 : 2 : 4 :
 cycle [2,4,2,4,6,2,6,4,2,4,6,6,2,6,4,2,6,4,6,8,4,2,4,2,4,8,6,4,6,2,4,6,2,6,6,4,2,4,6,2,6,4,2,4,2,10,2,10]
plist = scanl1 (+) dlist
primeList = filter isPrime plist

-- ===========================================================
-- Tests if a number is prime to a single divisor
-- or to  a list of divisors
-- -----------------------------------------------------------
class PrimeTo a where
    isPrTo :: Integer -> a -> Bool
instance PrimeTo Integer where
    isPrTo n d = 0 /= (rem n d)
instance PrimeToList a => PrimeTo [a] where
    isPrTo = isPrToList

class PrimeToList a where
    isPrToList :: Integer -> [a] -> Bool
instance PrimeToList Integer where
    isPrToList n = all $ isPrTo n

-- ===========================================================
-- Shifts a 1 bit up until greater than a value n
-- Result > n > Result / 4
-- -----------------------------------------------------------
bitmatch ::  Integer -> Integer -> Integer
bitmatch n b
    | n < b = b
    | otherwise = bitmatch n (b * 4)

-- ===========================================================
-- Top-level square root function, uses srqhelper as a helper
-- iSqrem: Returns (root, remainder)
-- iSqrt:  Returns root
-- For iSqrt x,
--      x == root*root + remainder
--      x < (root+1) * (root+1)
-- -----------------------------------------------------------
iSqrem :: Integer -> (Integer,Integer)
iSqrem x = srqhelper x 0 (bitmatch x 1)
iSqrt ::  Integer -> Integer
iSqrt = fst . iSqrem

-- ===========================================================
-- Iterative-Recursive square root finder
-- remainder:   Initially subject, diminishes with progress
-- root:        Initially zero, develops with progress
-- onebit:      A single bit, starts at high end, shifts down with progress
-- -----------------------------------------------------------
srqhelper :: Integer -> Integer -> Integer -> (Integer,Integer)
srqhelper remainder root onebit
    | onebit < 1        = (root,remainder)
    | remainder >= 0    = srqhelper newrem (div newroot 2) (div onebit 4)
    | otherwise = error "Attempting to take square root of negative Integer"
        where
            try = root + onebit
            newrem = if remainder >= try then (remainder - try) else remainder
            newroot = if remainder >= try then (try + onebit) else root

-- ===========================================================
-- Tests if a number is prime
-- -----------------------------------------------------------
isPrime :: Integer -> Bool
isPrime x = firstFalse $ map (isPrTo x) (takeWhile (<= (iSqrt x)) plist)

-- ===========================================================
-- Searches a finite list for the first False value
-- -----------------------------------------------------------
firstFalse :: [Bool] -> Bool
firstFalse [] = True
firstFalse (x:xs) = x && (firstFalse xs)

-- ===========================================================
-- Searches a finite list for the first True value
-- -----------------------------------------------------------
firstTrue :: [Bool] -> Bool
firstTrue [] = False
firstTrue (x:xs) = x || (firstTrue xs)

-- ===========================================================
-- Factorization
-- Given an integer, returns a list of its prime factors
-- number:  Number to be factored; ground down as factoring proceeds
-- result:  Factor list; builds up as factoring proceeds
-- divlist: List of possible prime factors, worn away during operation
-- For efficiency, intermediate factor list is built in reverse order
-- -----------------------------------------------------------
primeFactors :: Integer -> [Integer]
primeFactors number = reverse $ factorhelper number [] plist

factorhelper :: Integer -> [Integer] -> [Integer] -> [Integer]
factorhelper number result divlist@(p:ps)
    | number < 2        = result
    | isPrime number    = number : result
    | isPrTo number p   = factorhelper number result ps
    | otherwise         = factorhelper (div number p) (p : result) divlist

-- ===========================================================
