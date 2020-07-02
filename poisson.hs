import System.Random
import Numeric

ndp = 4
bround :: (Integral b) => b -> Double -> Double
bround places x = (fromIntegral (round ( x * exp))) / exp
       where exp = 10.0 ^ places
--bround x places = x

rndp = (bround ndp)

inRand :: (RandomGen g) => g -> [Double]
inRand gg = next:(inRand gg')
        where   instance Double next
                (next,gg') = random gg

testRand = inRand (mkStdGen 1278267)

limitedPoissonStream :: (RandomGen g) => Double -> Double -> Double -> g -> [Double]
limitedPoissonStream rate start limit gen
        | next > limit = []
        | otherwise     = next:(limitedPoissonStream rate next limit newGen)
        where  (rvalue, newGen) = random gen
               next = start - log(rvalue) / rate

countedPoissonStream :: (RandomGen g) => Double -> Double -> Int -> g -> [Double]
countedPoissonStream rate start count gen
        | count <= 0 = []
        | otherwise     = next:(countedPoissonStream rate next (count-1) newGen)
        where  (rvalue, newGen) = random gen
               next = rndp (start - log(rvalue) / rate)

infinitePoissonStream :: (RandomGen g) => Double -> Double -> g -> [Double]
infinitePoissonStream rate start gen = next:(infinitePoissonStream rate next newGen)
        where  (rvalue, newGen) = random gen
               next = rndp (start - log(rvalue) / rate)

infinitePoissonValues :: (RandomGen g) => Double -> g -> [Double]
infinitePoissonValues rate gen = next:(infinitePoissonValues rate newGen)
        where  (rvalue, newGen) = random gen
               next = rndp (0.0 - log(rvalue) / rate)

mmQueue :: Int -> Double -> Double -> Int -> [(Double,Double)]
mmQueue count arr serv seed =
        zip ( countedPoissonStream arr 0.0 count (mkStdGen seed) )
            (infinitePoissonValues serv ( mkStdGen (seed+93141) ) )

eventLog :: [(Double,Double)] -> [(Double,Double,Double,Double,Double,Double)]
eventLog queue = ieventLog 0.0 queue

ieventLog :: Double -> [(Double,Double)] -> [(Double,Double,Double,Double,Double,Double)]
ieventLog start [] = []
ieventLog start (q:qs) = (tarr, twait, transit, sidle, sstart, sserv) : (ieventLog newstart qs) where
          (tarr, sserv) = q
          sstart = max start tarr
          sidle = rndp (sstart - start)
          twait = rndp (sstart - tarr)
          transit = rndp (twait + sserv)
          newstart = rndp (sstart + sserv)

printAll :: [(Double,Double,Double,Double,Double,Double)] -> IO ()
printAll []     = return ()
printAll (x:xs) = do putStrLn (showX x)
                     printAll xs

show4 x = showFFloat (Just 4) x ","

showX :: (Random r, RealFloat r) => (r,r,r,r,r,r) -> String
showX x = (show4 a) ++ (show4 b) ++ (show4 c) ++ (show4 d) ++ (show4 e) ++ (show4 f) where
      (a,b,c,d,e,f) = x

main = do
       printAll (eventLog ( mmQueue 100 0.15 0.52 2841267 ) )

