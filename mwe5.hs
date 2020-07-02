someg :: (Integral a) => a -> a -> a -> a -> [(a,a)]
someg _ 0 _ _ = []
someg c c' n a = r ++ someg c (c'-1) (n+1) (a+g) 
    where   g = gcd n a
            r = if g == 1
            then []
            else [(c-c',g)]
