import System.Random
import Numeric

bround :: (RealFloat r, Integral b) => b -> r -> r
bround places x = (fromIntegral (round ( x * exp))) / exp
       where exp = 10.0 ^ places

rndp = (bround 4)

myGen = (mkStdGen 1278267)

infinitePoissonStream :: (RandomGen g, Random r, RealFloat r) => r -> r -> g -> [r]
infinitePoissonStream rate start gen = next:(infinitePoissonStream rate next newGen)
        where  (rvalue, newGen) = random gen
               next = (start - log(rvalue) / rate)

printAll :: (RealFloat r) => [r] -> IO ()
printAll []     = return ()
printAll (x:xs) = do putStrLn (showFFloat (Just 8) x "")
                     printAll xs

main = do
       printAll (take 10 (infinitePoissonStream 1.0 0.0 myGen ) :: [Double])
