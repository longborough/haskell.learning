import System.Random
import Numeric

bround :: Int -> Double -> Double
bround places x = (fromIntegral (round ( x * e))) / e
       where e = 10.0 ^ places

rndp :: Double -> Double
rndp = (bround 4)

-- myGen :: StdGen
-- myGen = (mkStdGen 1278267)
-- myGen = do
    -- g <- newStdGen
    -- return g

infinitePoissonInterval :: Double -> StdGen -> [Double]
infinitePoissonInterval rate gen = next : infinitePoissonInterval rate newGen
        where  (rvalue, newGen) = random gen
               next = rndp (-log(rvalue) / rate)

infinitePoissonStream :: Double -> Double -> StdGen -> [Double]
infinitePoissonStream rate start gen = next : infinitePoissonStream rate next newGen
        where  (rvalue, newGen) = random gen
               next = rndp (start - log(rvalue) / rate)

printAll :: [Double] -> IO ()
printAll []     = return ()
printAll (x:xs) = do 
    putStrLn (showFFloat (Just 8) x "")
    printAll xs

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)      
    
main = do
    myGen <- newStdGen
    printAll $ take 10 (infinitePoissonStream 0.1 0.0 myGen :: [Double])
    printAll $ take 10 (infinitePoissonInterval 0.1 myGen :: [Double])
