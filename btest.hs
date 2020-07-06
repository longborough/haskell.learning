import System.TimeIt
import Ntutils

multi n = (take 1) $ (filter isPrime) (map (\x -> 2^n + x) [1..])

main = do
    timeIt $ print $ multi 40
    timeIt $ print $ multi 45
    timeIt $ print $ multi 50
    timeIt $ print $ multi 55
