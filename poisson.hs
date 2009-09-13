import System.Random
import Numeric

ndp = 4
bround :: (RealFloat a, Integral b) => a -> b -> a
bround x places = (fromIntegral (round ( x * exp))) / exp 
       where exp = 10.0 ^ places
--bround x places = x

limitedPoissonStream :: ( Random r, RealFloat r, RandomGen g) => r -> r -> r -> g -> [r]
limitedPoissonStream rate start limit gen 
        | next > limit = [] 
        | otherwise     = next:(limitedPoissonStream rate next limit newGen)
        where  (rvalue, newGen) = random gen
               next = start - log(rvalue) / rate  

countedPoissonStream :: ( Random r, RealFloat r, RandomGen g ) => r -> r -> Int -> g -> [r]
countedPoissonStream rate start count gen 
        | count <= 0 = [] 
        | otherwise     = next:(countedPoissonStream rate next (count-1) newGen)
        where  (rvalue, newGen) = random gen
               next = bround (start - log(rvalue) / rate) ndp  

infinitePoissonStream :: ( Random r, RealFloat r, RandomGen g) => r -> r -> g -> [r]
infinitePoissonStream rate start gen = next:(infinitePoissonStream rate next newGen)
        where  (rvalue, newGen) = random gen
               next = bround (start - log(rvalue) / rate) ndp
               
infinitePoissonValues :: ( Random r, RealFloat r, RandomGen g) => r -> g -> [r]
infinitePoissonValues rate gen = next:(infinitePoissonValues rate newGen)
        where  (rvalue, newGen) = random gen
               next = bround (0.0 - log(rvalue) / rate) ndp

mmQueue :: Int -> Double -> Double -> Int -> [(Double,Double)]
mmQueue count arr serv seed = 
        zip ( countedPoissonStream arr 0.0 count (mkStdGen seed) ) 
            (infinitePoissonValues serv ( mkStdGen (seed+93141) ) )

eventLog :: (Random r, RealFloat r) => [(r,r)] -> [(r,r,r,r,r,r)]
eventLog queue = ieventLog 0.0 queue

ieventLog :: (Random r, RealFloat r) => r -> [(r,r)] -> [(r,r,r,r,r,r)]
ieventLog start [] = []
ieventLog start (q:qs) = (tarr, twait, transit, sidle, sstart, sserv) : (ieventLog newstart qs) where
          (tarr, sserv) = q
          sstart = max start tarr
          sidle = bround(sstart - start) ndp
          twait = bround (sstart - tarr) ndp
          transit = bround (twait + sserv) ndp
          newstart = bround (sstart + sserv) ndp

printAll :: (Random r, RealFloat r) => [(r,r,r,r,r,r)] -> IO ()
printAll []     = return ()
printAll (x:xs) = do putStrLn (showX x)
                     printAll xs          

show4 x = showFFloat (Just 4) x ","

showX :: (Random r, RealFloat r) => (r,r,r,r,r,r) -> String
showX x = (show4 a) ++ (show4 b) ++ (show4 c) ++ (show4 d) ++ (show4 e) ++ (show4 f) where
      (a,b,c,d,e,f) = x

main = do
       printAll (eventLog ( mmQueue 100 0.15 0.52 2841267 ) ) 

