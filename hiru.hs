import Data.List
primes :: [Integer]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]

--(\\) :: Eq a => [a] -> [a] -> [a]
--xs \\ ys = foldl (\zs y -> delete y zs) xs ys
  
nums = [2..99]
plist = [ x | x <- take 100 primes, x <= 100 ] 

pairs = [ [x,y,x+y,x*y] | x <- nums, y <- nums, (x + y) <= 100, x < y, (x*x) /= y, (notElem x plist) || (notElem y plist)]

ix :: Int -> [x] -> x
ix i xs = xs !! i

lcount :: [x] -> Int
lcount xs = (length xs) - 1

sums = map (ix 2) pairs
ksum = map lcount (group ( sort ((++) sums [0 .. maximum sums] ) ) )

prods = map (ix 3) pairs
kprod = map lcount (group ( sort ((++) prods [0 .. maximum prods] ) ) )

