ack m n =
        case (m,n) of
             (0,_) -> n+1
             (_,0) -> ack (m-1) 1
             (_,_) -> ack (m-1) (ack m (n-1))

-- ack 0 n = n+1
-- ack m 0 = ack (m-1) 1
-- ack m n = ack (m-1) (ack m (n-1))

fib 1 = 1
fib 2 = 1
fib n = (fib (n-1)) + (fib (n-2)) 

my_mult a 1 = a
my_mult a b = a + my_mult a (b-1)

-- my_map f [] = []  
-- my_map f x:xs = (f x) : my_map f xs
  