import Data.Bits

plist = 0 : 2 : 1 : cycle [2,2,4,2,4,2,4,6,2,6]

bitmatch :: (Ord a, Bits a) => a -> a -> a
bitmatch n b
    | n < b = b
    | otherwise = bitmatch n (shift b 2)

isqrt :: (Integral a, Ord a, Bits a) => a -> (a,a)
isqrt x = sqriter x x 0 (bitmatch x 1)

sqriter :: (Integral a, Ord a, Bits a) => a -> a -> a -> a -> (a,a)
sqriter i residue root obit
    | obit < 1 = (root,residue)
    | otherwise = sqriter i nres (shiftR nroot 1) (shiftR obit 2) where
        x = root + obit
        nres = if residue >= x then (residue - x) else residue
        nroot = if residue >= x then (x + obit) else root

printem :: [(Int, Int)] -> IO ()
printem [] = return ()
printem (x:xs) = do
    putStrLn(showInt(fst x) ++ " " + showInt(snd x))
    printem xs

main = do
    printem map isqrt [1..20]
