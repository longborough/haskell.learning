{-# LANGUAGE FlexibleInstances #-}
import Isqrt

printem ::  [(Integer,[Integer])] -> IO ()
printem [] = return ()
printem (x:xs) = do
    print x 
    printem xs

-- ===========================================================
-- Two useful lists
-- dlist:   List of increments between possible primes
-- plist:   Running total = list of possible primes
-- x is prime => x is a member of plist 
-- -----------------------------------------------------------
dlist = 2 : 1 : 2 : 2 : cycle [4,2,4,2,4,6,2,6]
--      2,1,2,2,4,2,4,2,4,6,2,6,4,2,4,6,6,2,6,4,2,6,4,6,8,4,2,4,2,4,8]
plist = scanl1 (+) dlist

-- ===========================================================
-- Tests if a number is prime to a single divisor
-- or to  a list of divisors
-- -----------------------------------------------------------
class PrimeTo a where
    ispt :: Integer -> a -> Bool
instance PrimeTo (Integer) where
    ispt n d = 0 /= (rem n d)
instance PrimeTo ([Integer]) where
    ispt n [] = True
    ispt n (x:xs) = (ispt n x) && (ispt n xs)

-- ===========================================================
-- Tests if a number is prime
-- -----------------------------------------------------------
isprime :: Integer -> Bool
isprime x = p where
    p = firstfalse $ map (ispt x) ( upto (isqrt x) [] plist ) 

-- ===========================================================
-- Searches a finite list for the first False value
-- -----------------------------------------------------------
firstfalse :: [Bool] -> Bool
firstfalse [] = True
firstfalse (x:xs) = x && (firstfalse xs)

-- ===========================================================
-- Searches a finite list for a True value
-- -----------------------------------------------------------
firsttrue :: [Bool] -> Bool
firsttrue [] = False
firsttrue (x:xs) = x || (firsttrue xs)

-- ===========================================================
-- Extracts everything up to a given number 
-- from an infinite, ordered list of integers
-- -----------------------------------------------------------
upto :: Integer -> [Integer] -> [Integer] -> [Integer]
upto _ _ [] = []
upto n r (x:xs)
    | x > n = r
    | otherwise = upto n (r ++ [x]) xs
    
factor :: Integer -> [Integer]
factor n = tail $ reverse $ getfactors n [1] plist

getfactors :: Integer -> [Integer] -> [Integer] -> [Integer]    
getfactors n r all@(p:ps)
    | n < 2 = r
    | isprime n = n : r
    | ispt n p = getfactors n r ps
    | otherwise =  getfactors (div n p) (p : r) all

-- ===========================================================
-- Experimental stuff 
-- -----------------------------------------------------------
l = filter isprime plist
z = zip (tail plist) plist

-- delve :: [Integer] -> [Integer]
-- delve n = l where
    -- c = filter (\x -> (ispt x n )) [2..]
    -- d = (upto (last n) [] $ 0:plist)  ++ c
    -- l = map (\x -> (fst x) - (snd x)) $ zip (tail d) d
    
    
main = do
    printem $ zip [1..100] ( map factor [1..100] )
    -- printem $ take 400 $ zip (delve [2,3,5,7,11]) (scanl1 (+) $ delve [2,3,5,7,11])
    -- print $ factor $ 2147483647
    