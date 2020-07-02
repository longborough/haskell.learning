import System.Random
import Numeric

inRand :: (RandomGen g, Random r, RealFloat r) => g -> [r]
inRand gg = next:(inRand gg')
        where (next,gg') = random gg

testRand = inRand (mkStdGen 1278267)

printAll :: [Double] -> IO ()
printAll []     = return ()
printAll (x:xs) = do putStrLn (showFFloat (Just 4) x "")
                     printAll xs



main = do
       printAll (take 10 testRand)
