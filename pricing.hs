dpw = [1..5]
lorate = [640,560,480,480,480]
hirate = [800,720,640,560,560]
rates = map (\(a, (b, c)) -> (a,b,c)) (zip dpw (zip hirate lorate))
 
weeks = [1..7]
thrweeks = [(a,a+b) | a <- weeks, b<-[0..2]] 

rlist = [(nw,th,dp,if nw<th then hr else lr) | (nw,th)<-thrweeks,(dp,hr,lr)<-rates]
xlist = [(nw,th,dp,dr,nw*dp,nw*dp*dr) | (nw,th,dp,dr) <- rlist]
filtered tt = [((nw,th,dp,dr,td,tp),(nwx,thx,dpx,drx,tdx,tpx)) | (nw,th,dp,dr,td,tp) <- xlist, (nwx,thx,dpx,drx,tdx,tpx) <- xlist, th == thx, tp <= tpx, td >= tdx, (tp < tpx) || (td > tdx) , nw /= nwx, dp <= dpx, th == tt ]

format [] = ""        
format (x:xs) = (show x) ++ "\n" ++ (format xs)
