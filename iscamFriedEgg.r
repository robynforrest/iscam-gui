##############################################################
fried.egg <- function(xx,yy,...){
	bw <- 50
	bwx <- diff(extendrange(xx))/bw
  bwy <- diff(extendrange(yy))/bw
	#print(c(bwx, bwy))
	#bwx=(max(xx)-min(xx))/bw
	#bwy=(max(yy)-min(yy))/bw
	est <- bkde2D(cbind(xx,yy),bandwidth=c(bwx,bwy))#,gridsize=c(81, 81))
	est$fhat <- est$fhat/max(est$fhat)
	#plot(xx,yy,pch=".",col="dark grey",xlab=NA,ylab=NA,type="n")
	#text(max(xx),max(yy),labels="D",adj=c(1,1))
	lvs <- c(0.025,0.25,0.75,0.975)
	maxct <- max(lvs)
	nlvs <- length(lvs)
	thelines <- contourLines(est$x1,est$x2,est$fhat,levels=lvs)
	polygon(thelines[[nlvs-3]]$x,thelines[[nlvs-3]]$y,col="khaki",border="khaki",lwd=1)
	polygon(thelines[[nlvs-2]]$x,thelines[[nlvs-2]]$y,col="snow",border="snow1",lwd=2)
	polygon(thelines[[nlvs-1]]$x,thelines[[nlvs-1]]$y,col="yellow",border="yellow2",lwd=3)
	polygon(thelines[[nlvs]]$x,thelines[[nlvs]]$y,col="lightyellow",border="yellow",lwd=1)
	#contour(est$x1,est$x2,est$fhat,drawlabels=T,add=T,levels=lvs,lty=1,lwd=1,labcex= 0.7)
	#Add salt and pepper
	#xi=sample(1:length(xx),300)
	#points(xx[xi],yy[xi],pch=".",col=grey(0:10/10))
} 

panel.hist <- function(x, ...){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="tan", ...)
}

bi.level <- function(xx,yy,...){
	bw <- 30
	bwx <- diff(extendrange(xx))/bw; bwy=diff(extendrange(yy))/bw
	#print(c(bwx, bwy))
	#bwx=(max(xx)-min(xx))/bw
	#bwy=(max(yy)-min(yy))/bw
	est <- bkde2D(cbind(xx,yy),bandwidth=c(bwx,bwy))#,gridsize=c(81, 81))
	est$fhat <- est$fhat/max(est$fhat)
	#plot(xx,yy,pch=".",col="dark grey",xlab=NA,ylab=NA,type="n")
	#text(max(xx),max(yy),labels="D",adj=c(1,1))
	lvs <- c(0.05,0.25,0.5, 0.75,0.95)
	maxct <- max(lvs)
	nlvs <- length(lvs)
	thelines <- contourLines(est$x1,est$x2,est$fhat,levels=lvs)
	#polygon(thelines[[nlvs-3]]$x,thelines[[nlvs-3]]$y,col="khaki",border="khaki",lwd=1)
	#polygon(thelines[[nlvs-2]]$x,thelines[[nlvs-2]]$y,col="snow",border="snow1",lwd=2)
	#polygon(thelines[[nlvs-1]]$x,thelines[[nlvs-1]]$y,col="yellow",border="yellow2",lwd=3)
	#polygon(thelines[[nlvs]]$x,thelines[[nlvs]]$y,col="lightyellow",border="yellow",lwd=1)
	contour(est$x1,est$x2,est$fhat,drawlabels=F,add=T,levels=lvs,lty=1,lwd=1,labcex= 0.7)
	#Add salt and pepper
	#xi=sample(1:length(xx),300)
	#points(xx[xi],yy[xi],pch=".",col=grey(0:10/10))
}
