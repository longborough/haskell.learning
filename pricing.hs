dpw = [1..5]
lorate = [640,560,480,480,480]
hirate = [800,720,640,560,560]
rates = map (\(a, (b, c)) -> (a,b,c)) (zip dpw (zip hirate lorate))
 
weeks = [1..7]
thrdays = [(a,b) | a <- weeks, b<-[10,12,15,18,20]] 

rlist = [(nw,th,dp,if (nw*dp)<th then hr else lr) | (nw,th)<-thrdays,(dp,hr,lr)<-rates]
xlist = [(nw,th,dp,dr,nw*dp,nw*dp*dr) | (nw,th,dp,dr) <- rlist]
filtered tt = [((nw,th,dp,dr,td,tp),(nwx,thx,dpx,drx,tdx,tpx)) | (nw,th,dp,dr,td,tp) <- xlist, (nwx,thx,dpx,drx,tdx,tpx) <- xlist, th == thx, tp <= tpx, td >= tdx, (tp < tpx) || (td > tdx) , nw /= nwx, th == tt ]

format [] = ""        
format (x:xs) = (show x) ++ "\n" ++ (format xs)
