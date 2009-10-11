p1 = (2.0, 3.0)
p2 = (4.0, 4.0)
p0l = (1.0, 4.0)
p0x = (0.0, 2.0)
p0r = (4.0, 1.0)

delta :: (Double,Double) -> (Double,Double) -> (Double,Double)
delta (x1,y1) (x2,y2) = (x2-x1,y2-y1)

coeff :: (Double,Double) -> (Double,Double) -> (Double,Double) 
coeff (x1,y1) (x2,y2) = ( a , b ) where
    a = (y2 - y1) / (x1 * y2 - x2 * y1)
    b = (x2 - x1) / (y1 * x2 - y2 * x1)

value :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double,String,String,String)
value (x1,y1) (x2,y2) (x0,y0) =  (x0, y0, l, c, r ) where
    (a,b) = coeff (x1,y1) (x2,y2)
    v = a * x0 + b * y0 - 1
    l = if ( v < (-0.1e-8) ) then show y0 else ""
    c = if ( v < 0.1e-8 ) && ( v > (-0.1e-8) ) then show y0 else ""
    r = if ( v > 0.1e-8 )    then show y0 else ""
