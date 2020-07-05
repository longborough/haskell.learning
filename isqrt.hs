module Isqrt (isqrt, isqrem) where

-- ===========================================================
-- Shifts a 1 bit up until greater than a value n
-- -----------------------------------------------------------
bitmatch ::  Integer -> Integer -> Integer
bitmatch n b
    | n < b = b
    | otherwise = bitmatch n (b * 4)

-- ===========================================================
-- Top-level square root function, uses sqriter as a helper
-- Returns (root, remainder)
-- For isqrt x,
--      x == root*root + remainder
--      x < (root+1) * (root+1)
-- -----------------------------------------------------------
isqrem :: Integer -> (Integer,Integer)
isqrem x = sqriter x 0 (bitmatch x 1)

-- ===========================================================
-- Top-level square root function, uses sqriter as a helper
-- Returns root (without remainder)
-- For isqrt x,
--      root * root <= x < (root+1) * (root+1)
-- -----------------------------------------------------------
isqrt ::  Integer -> Integer
isqrt = fst . isqrem

-- ===========================================================
-- Iterative-Recursive square root finder
-- remainder:   Initially subject, diminishes with progress
-- root:        Initially zero, develops with progress
-- onebit:      A single bit, starts at high end, shifts down with progress
-- -----------------------------------------------------------
sqriter :: Integer -> Integer -> Integer -> (Integer,Integer)
sqriter remainder root onebit
    | onebit < 1 = (root,remainder)
    | otherwise = sqriter nres (div nroot 2) (div onebit 4) where
        try = root + onebit
        nres = if remainder >= try then (remainder - try) else remainder
        nroot = if remainder >= try then (try + onebit) else root

-- ===========================================================
-- Printer
-- -----------------------------------------------------------
printem :: [(Int, Int)] -> IO ()
printem [] = return ()
printem (x:xs) = do
    let s = fst x
    let r = snd x
    print (s*s+r, s, r)
    printem xs

-- ===========================================================
