import Data.List ((\\), elemIndex, intersect, nub)
import Data.Bifunctor (bimap, first)

combs 0 _ = [[]]
combs _ [] = []
combs k (x:xs) = ((x :) <$> combs (k - 1) xs) ++ combs k xs

depLibs :: [(String, String)]
depLibs =
  [ ( "pants", [])
  , ( "trousers", "pants")
  , ( "trousers", "socks")
  , ( "shirt", "undershirt")
  , ( "watch", "shirt")
  , ( "waistcoat", "shirt")
  , ( "tie", "shirt")
  , ( "belt", "trousers")
  , ( "belt", "shirt")
  , ( "shoes", "socks")
  , ( "shoes", "trousers")
  , ( "jacket", "waistcoat")
  ]

toposort :: [(String, String)] -> [String]
toposort xs
  | (not . null) cycleDetect =
    error $ "Dependency cycle detected for libs " ++ show cycleDetect
  | otherwise = foldl makePrecede [] dB
  where
    dB = (\(x, y) -> (x, y \\ x)) . bimap return words <$> xs
    makePrecede ts ([x], xs) =
      nub $
      case elemIndex x ts of
        Just i -> uncurry (++) $ first (++ xs) $ splitAt i ts
        _ -> ts ++ xs ++ [x]
    cycleDetect =
      filter ((> 1) . length) $
      (\[(a, as), (b, bs)] -> (a `intersect` bs) ++ (b `intersect` as)) <$>
      combs 2 dB

main :: IO ()
main = print $ toposort depLibs
