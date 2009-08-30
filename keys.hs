import System.Random
fourtimes :: a -> [a]
fourtimes x = [x,x,x,x]
alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
--rchar = do
--    x <- getStdRandom $ 
--    return x
rchars gen = rchar:rchars newGen where
    (rchar,newGen) = randomR ('A','Z') gen    

--main = do
--   gen <- newStdGen
--   print take 16 (rchars gen) 
