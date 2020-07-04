module Isqrt (isqrt, isqrem) where
import Data.Bits

-- ===========================================================
-- Shifts a 1 bit up until greater than a value n
-- -----------------------------------------------------------
bitmatch :: (Ord a, Bits a) => a -> a -> a
bitmatch n b
    | n < b = b
    | otherwise = bitmatch n (shift b 2)

-- ===========================================================
-- Top-level square root function, uses sqriter as a helper
-- Returns (root, remainder)
-- For isqrt x,
--      x == root*root + remainder
--      x < (root+1) * (root+1)
-- -----------------------------------------------------------
isqrem :: (Integral a, Ord a, Bits a) => a -> (a,a)
isqrem x = sqriter x 0 (bitmatch x 1)

-- ===========================================================
-- Top-level square root function, uses sqriter as a helper
-- Returns root (without remainder)
-- For isqrt x,
--      root * root <= x < (root+1) * (root+1)
-- -----------------------------------------------------------
isqrt :: (Integral a, Ord a, Bits a) => a -> a
isqrt = fst . isqrem

-- ===========================================================
-- Iterative-Recursive square root finder
-- remainder:   Initially subject, diminishes with progress
-- root:        Initially zero, develops with progress
-- onebit:      A single bit, starts at high end, shifts down with progress
-- -----------------------------------------------------------
sqriter :: (Integral a, Ord a, Bits a) => a -> a -> a -> (a,a)
sqriter remainder root onebit
    | onebit < 1 = (root,remainder)
    | otherwise = sqriter nres (shiftR nroot 1) (shiftR onebit 2) where
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
