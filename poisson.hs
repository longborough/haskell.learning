import System.Random
ndp = 4
bround :: (RealFloat a, Integral b) => a -> b -> a
bround x places = (fromIntegral (round ( x * exp))) / exp 
       where exp = 10.0 ^ places

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

mmQueue :: (Random r, RealFloat r) => Int -> r -> r -> Int -> [(r,r)]
mmQueue count arr serv seed = 
        zip ( countedPoissonStream arr 0.0 count (mkStdGen seed) ) 
            (infinitePoissonValues serv ( mkStdGen (seed+93141) ) )

eventLog :: (Random r, RealFloat r) => [(r,r)] -> [(r,r,r,r)]
eventLog queue = ieventLog 0.0 0.0 queue

ieventLog :: (Random r, RealFloat r) => r -> r -> [(r,r)] -> [(r,r,r,r)]
ieventLog start end [] = []
ieventLog start end (q:qs) = (arr, serv, newstart, newend) : (ieventLog newstart newend qs) where
          (arr, serv) = q
          newstart = max end arr
          newend = bround (newstart + serv) ndp
