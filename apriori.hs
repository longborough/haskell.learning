module Main where

import qualified Data.Set as S
import Data.Maybe
import Data.List

type ItemSet a = S.Set a

type Transaction a = S.Set a

type Database a = S.Set (Transaction a)

data Item = AirfixPlane | Barbie | CuddlyToy | Dinosaur deriving (Enum,Eq,Ord,Show,Bounded)
simpleDatabase = S.fromList $ map S.fromList [[AirfixPlane,CuddlyToy,Dinosaur]
                                            , [Dinosaur,CuddlyToy]
                                            , [Barbie,CuddlyToy]
                                            , [AirfixPlane,Barbie,CuddlyToy,Dinosaur]]

apriori ::  (Enum a, Ord a,Bounded a) => Database a -> Int -> [(ItemSet a,Int)] -> [(ItemSet a ,Int)] -> [(ItemSet a,Int)]
apriori ts freq lk0 ls = let
              -- Generate next set of candidates
              ck = generate lk0

               -- One pass over the data to determine the support for each candidate
              ck' = S.fold (\t ck -> map (\(c,cnt) -> if S.isSubsetOf c t then (c,cnt+1) else (c,cnt)) ck) ck ts

              -- Filter the candidates that don't meet minimum support to form lk1
              lk1 = filter (\(c,cnt) -> cnt >= freq) ck'

              in case lk1 of
                         [] -> ls
                         _ -> apriori ts freq lk1 (lk1 ++ ls)

generate :: (Enum a, Ord a,Bounded a) => [(ItemSet a,Int)] -> [(ItemSet a ,Int)]
generate [] = map (\i -> (S.singleton i,0)) [minBound .. maxBound]
generate lk = let ss =  nub $ catMaybes $ map (\(x,y) -> join x y) ([ (x,y) | x <- lks, y <- lks])
                  lks = map fst lk
              -- Filter based on all k-1 subsets are in lk
             in map (\is -> (is,0)) $ filter (\s -> and $ map (\s' -> elem s' lks) (subSets s)) ss

join :: Ord a => ItemSet a -> ItemSet a -> Maybe  (ItemSet a)
join i1 i2 = let i3 = S.union i1 i2
             in case S.size i3 == S.size i1 + 1 of
                  True -> Just i3
                  False -> Nothing

subSets :: Ord a => S.Set a -> [S.Set a]
subSets xs = S.toList $ S.map (\x -> S.delete x xs) xs

main = putStrLn $ show $ (apriori simpleDatabase 2 [] [])
