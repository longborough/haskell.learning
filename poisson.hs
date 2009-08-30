import System.Random
poissonStream :: ( Random r, RealFloat r, RandomGen g) => r -> r -> r -> g -> [r]
poissonStream rate start limit gen 
        | next > limit = [] 
        | otherwise     = next:(poissonStream rate next limit newGen)
        where  (rvalue, newGen) = random gen
               next = start - log(rvalue) / rate  
