import Isqrt

printem ::  [Int] -> IO ()
printem [] = return ()
printem (x:xs) = do
    print x 
    printem xs

dlist = 2 : 1 : cycle [2,2,4,2,4,2,4,6,2,6]
plist = scanl1 (+) dlist

isr0 :: Int -> Int -> Bool
isr0 n d = 0 == (rem n d)

isprime :: Int -> Bool
isprime x = p where
    p = not . firsttrue $ map (isr0 x) ( upto (isqrt x) [] plist ) 
    
firsttrue :: [Bool] -> Bool
firsttrue [] = False
firsttrue (x:xs) = x || (firsttrue xs)

upto :: Int -> [Int] -> [Int] -> [Int]
upto _ _ [] = []
upto n r (x:xs)
    | x > n = r
    | otherwise = upto n (r ++ [x]) xs

l = filter isprime plist
z = zip (tail plist) plist
