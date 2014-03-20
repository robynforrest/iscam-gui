abc.impact <- function(admbDir="",type="wmf"){                
	#Run the 6 alternative data configurations and compare ABC estimates 
	op <- par(no.readonly=T)
	par(mfcol=c(1, 1))   
	ABC <- vector()
  theta <<- NULL
  fn <- paste(admbDir,"iscam.rep",sep="")
	for(i in 1:7){
    arg <- paste(admbDir,"iscam -est -nox -model ",i,sep="")
		system(arg)
		M <- reptoRlist(fn)
		ABC[i] <- M$ABC[2]
		theta <<- rbind(theta, c(MSY=M$MSY, Q=M$q, FMSY=M$Fmsy,M= M$M, ABC=M$ABC[2]))
	}                            
	models <- c("C","CA","CI","CAI","CIS","CAS","CAIS")
	barplot(ABC*1000, names.arg=models, ylab="2010 ABC values (1000 mt)", xlab="Stock Information")
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "ABC.impact.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=7.5,height=5)
    }
    else if(type=="wmf"){
      filename <- "ABC.impact.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	barplot(t(theta), beside=T, names.arg=models,legend.text=T,ylim=c(0,.7), xlab="Stock Information", ylab="Parameter or ABC value")
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "ABCpar.impact.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=7.5,height=5)
    }
    else if(type=="wmf"){
      filename <- "ABCpar.impact.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	par(op)
}

sim.exp <- function(n=10,type="wmf"){
  fn <- paste(admbDir,"history.txt",sep="")
	file.remove(fn)
	for(i in 1:n){
		arg<-paste(admbDir,"iscam -est -nox -sim ",849+2*(i-1),sep="")
		system(arg,invisible=F)
	}
	d <- read.table(fn,header=F)
	par(mfcol <- c(1,1))
	names(d) <- c("UID","Phase","MSY","Fmsy","M","ah","ghat","abar","gbar","rho","varphi")
	boxplot(d[d$Phase==1,-c(1:2)],ylim=c(-1,1),col="salmon")
	boxplot(d[d$Phase==2,-c(1:2)],ylim=c(-1,1),col="orange")
	boxplot(d[d$Phase==3,-c(1:2)],ylim=c(-1,1),col="tan", ylab="log2 bias ratio") 
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "Sim.exp.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=7.5,height=5)
    }
    else if(type=="wmf"){
      filename <- "Sim.exp.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	#pairs(d[d$Phase==3,-c(1:2)])
	#boxplot(d[d$Phase==3,-c(1:2)],col="orange")
}

fig.steepness<-function(type="wmf"){
	op <- par(no.readonly=T)
	par(mfcol=c(1,1),cex.axis=1)
	#NB t1 comes from Impliedprior.R
	hist(mc$h,breaks=25,xlim=c(0.2,1),main="",xlab="h",ylab="Density",prob=T)
	lines(density(t1,na.rm=T,from=0.2,to=1.0,adjust=2),col=2,lwd=2)
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamSteepness.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=7.5,height=5)
    }
    else if(type=="wmf"){
      filename <- "iscamSteepness.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	par(op)
}

prior.dist<-function(admbDir="",type="wmf"){
	op <- par(no.readonly=T)
	#x11(height=4,width=8)     
	#quartz(height=4, width=8)
	
	
	#RF CHANGE - NEXT 9 LINES COMMENTED OUT
	#x <- seq(0,1.,by=0.01)
	#par(mfcol=c(1,2),cex.axis=1.,cex.lab=1,mar=c(4,5,1,1))
	#p1 <- dlnorm(x,log(0.35),0.262)
	#p2 <- dlnorm(x,log(0.42),0.222)
	#p3 <- dlnorm(x,log(0.28),0.319)
	#p4 <- dlnorm(x,log(0.35),0.5)
	#p4 <- dlnorm(x,log(0.35),0.15)
	#matplot(x,cbind(p1,p2,p3,p4),type="l",col=1:4,lwd=2,lty=2,xlab="F",ylab="Prior density")
	#legend("topright",paste("mu=",c(0.35, 0.42, 0.28,0.35),"\nsig=",c(0.262,0.222,0.319,0.5)),lwd=2,bty="n",lty=2,col=1:4,cex=0.75)
	
	#RF CHANGE - CHANGE TO PRIORS FOR MSY
  par(mfcol=c(1,1))
	x <- seq(0,0.8,length=100)
	#p1 <- dnorm(x,0.200,0.5)
	p1 <- dlnorm(x,log(0.200),0.5)
	p2 <- dlnorm(x,log(0.2),0.2)
	p3 <- dlnorm(x,log(0.2),1.0)
	p4 <- dlnorm(x,log(0.500),0.2)
	matplot(x,cbind(p1,p2,p3,p4),type="l",col=1:4,lwd=2,lty=2,xlab="MSY",ylab="Prior density")
	legend("topright",paste("mu=",c(0.20, 0.2, 0.2,0.50),"\nsig=",c(0.5,0.2,1.0,0.2)),lwd=2,bty="n",lty=2,col=1:4,cex=1.5)
	
	if(logmsy==1){
		xx <- log(x) #seq(log(0.001),log(0.8),length=100)
		#p1 <- dnorm(x,0.200,0.5)
		p1 <- dnorm(xx,log(0.2),0.5)
		p2 <- dnorm(xx,log(0.2),0.2)
		p3 <- dnorm(xx,log(0.2),1.0)
		p4 <- dnorm(xx,log(0.5),0.2)
		matplot(xx,cbind(p1,p2,p3,p4),type="l",col=1:4,lwd=2,lty=2,xlab="ln MSY",ylab="Prior density")
		legend("topleft",paste("mu=",c(0.20, 0.2, 0.2,0.50),"\nsig=",c(0.5,0.2,1.0,0.2)),lwd=2,bty="n",lty=2,col=1:4,cex=1.5)
		
	}
	
	
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "PriorDist.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=8,height=5)
    }
    else if(type=="wmf"){
      filename <- "PriorDist.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
}

fprior <- function(type="wmf"){
	op <- par(no.readonly=T)
	par(mfcol=c(1,2),cex.axis=1.,cex.lab=1,mar=c(4,5,1,1))
	x <- seq(0,1.,by=0.01)
	p1 <- dlnorm(x,log(0.35),0.262)
	p2 <- dlnorm(x,log(0.42),0.222)
	p3 <- dlnorm(x,log(0.28),0.319)
	p4 <- dlnorm(x,log(0.35),0.5)
	
	matplot(x,cbind(p1,p2,p3),type="l",col=1:3,lwd=2,lty=2,xlab="F",ylab="Prior density")
	legend("topright",paste("mu = ",c(0.35, 0.42, 0.28)),lwd=2,bty="n",lty=2,col=1:3)
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamR4_1.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=8,height=5)
    }
    else if(type=="wmf"){
      filename <- "iscamR4_1.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	
	matplot(x,cbind(p1,p2,p3,p4),type="l",col=1:4,lwd=2,lty=2,xlab="F",ylab="Prior density")
	legend("topright",paste("mu=",c(0.35, 0.42, 0.28,0.35)," sig=",c(0.262,0.222,0.319,0.5)),lwd=2,bty="n",lty=2,col=1:4)
	## FIG	#######################################

	
	if(saveon){
    if(type=="eps"){
      filename <- "iscamR4_1b.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=8,height=5)
    }
    else if(type=="wmf"){
      filename <- "iscamR4_1b.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	
	par(mfcol=c(2,2),cex.axis=1.,cex.lab=1.,mar=c(5,5,1,1))
	plot(density(mcbase$fmsy,adjust=1.5,from=0.,to=0.7),lwd=2,col=1,main="",xlab="Fmsy",ylim=c(0,5))
	lines(density(mchighf$fmsy,adjust=1.5,from=0.,to=0.7),lwd=2,col=2,)
	lines(density(mclowf$fmsy,adjust=1.5,from=0.,to=0.7),lwd=2,col=3)
	lines(density(mc.comp$fmsy,adjust=1.5,from=0.,to=0.7),lwd=2,col=4)
	matlines(x,cbind(p1,p2,p3,p4),type="l",col=1:4,lwd=2,lty=2,xlab="F",ylab="Prior density")

	plot(density(mcbase$abc,adjust=1.5,from=0,to=1.5),lwd=2,col=1,main="",xlab="ABC",,ylim=c(0,2))
	lines(density(mchighf$abc,adjust=1.5,from=0,to=1.5),lwd=2,col=2)
	lines(density(mclowf$abc,adjust=1.5,from=0,to=1.5),lwd=2,col=3)
	lines(density(mc.comp$abc,adjust=1.5,from=0,to=1.5),lwd=2,col=4)
	
	plot(density(mcbase$h,adjust=1.5,from=0.2,to=1),lwd=2,col=1,main="",xlab="Steepness (h)",,ylim=c(0,6))
	lines(density(mchighf$h,adjust=1.5,from=0.2,to=1),lwd=2,col=2)
	lines(density(mclowf$h,adjust=1.5,from=0.2,to=1),lwd=2,col=3)
	lines(density(mc.comp$h,adjust=1.5,from=0.2,to=1),lwd=2,col=4)
	
	plot(density(mcbase$msy,adjust=1.5,from=0.,to=0.7),lwd=2,col=1,main="",xlab="MSY")
	lines(density(mchighf$msy,adjust=1.5,from=0.,to=0.7),lwd=2,col=2)
	lines(density(mclowf$msy,adjust=1.5,from=0.,to=0.7),lwd=2,col=3)
	lines(density(mc.comp$msy,adjust=1.5,from=0.,to=0.7),lwd=2,col=4)
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamR4_2.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=8,height=5)
    }
    else if(type=="wmf"){
      filename <- "iscamR4_2.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	plot(density(mcbase$depletion,adjust=1.5,from=0.,to=1.2),lwd=2,col=1,main="",xlab="Depletion",ylim=c(0,2.5))
	lines(density(mchighf$depletion,adjust=1.5,from=0.,to=1.2),lwd=2,col=2)
	lines(density(mclowf$depletion,adjust=1.5,from=0.,to=1.2),lwd=2,col=3)
	lines(density(mc.comp$depletion,adjust=1.5,from=0.,to=1.2),lwd=2,col=4)
		
	plot(density(mcbase$SBo,adjust=1.5,from=0.,to=8),lwd=2,col=1,main="",xlab="SBo",ylim=c(0,0.7))
	lines(density(mchighf$SBo,adjust=1.5,from=0.,to=8),lwd=2,col=2)
	lines(density(mclowf$SBo,adjust=1.5,from=0.,to=8),lwd=2,col=3)
	lines(density(mc.comp$SBo,adjust=1.5,from=0.,to=8),lwd=2,col=4)
	
	plot(density(mcbase$m,adjust=1.5,from=0.15,to=0.35),lwd=2,col=1,main="",xlab="M",ylim=c(0,20))
	lines(density(mchighf$m,adjust=1.5,from=0.15,to=0.35),lwd=2,col=2)
	lines(density(mclowf$m,adjust=1.5,from=0.15,to=0.35),lwd=2,col=3)
	lines(density(mc.comp$m,adjust=1.5,from=0.15,to=0.35),lwd=2,col=4)

	plot(density(mcbase$SPRfmsy,adjust=1.5,from=0.25,to=1),lwd=2,col=1,main="",xlab="SPR(Fmsy)")
	lines(density(mchighf$SPRfmsy,adjust=1.5,from=0.25,to=1),lwd=2,col=2)
	lines(density(mclowf$SPRfmsy,adjust=1.5,from=0.25,to=1),lwd=2,col=3)
	lines(density(mc.comp$SPRfmsy,adjust=1.5,from=0.25,to=1),lwd=2,col=4)
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamR4_2b.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=8,height=5)
    }
    else if(type=="wmf"){
      filename <- "iscamR4_2b.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	
	par(mfcol=c(1,1))
	plot.ecdf(mcbase$abc,xlab="ABC",col.vert=1,col.hor=1,xlim=c(0,1),main="",ylab="Cumulative density",do.points=F,verticals=T,lwd=2)
	plot.ecdf(mchighf$abc,xlab="ABC",col.vert=2,col.hor=2,add=T,do.points=F,verticals=T,lwd=2)
	plot.ecdf(mclowf$abc,xlab="ABC",col.vert=3,col.hor=3,add=T,do.points=F,verticals=T,lwd=2)
	abline(h=c(0.25,0.5,0.75))
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamR4_3.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=8,height=5)
    }
    else if(type=="wmf"){
      filename <- "iscamR4_3.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	#lines(density(mchighf$abc,adjust=1.5,from=0,to=1.5),lwd=2,col=2)
	#lines(density(mclowf$abc,adjust=1.5,from=0,to=1.5),lwd=2,col=3)
}

retro <- function(N=1,admbDir="",type="wmf"){
	#This function is a shell for calling iscam -retro year.
	if(!exists("Retro")){
		Retro<-list()
		for(i in 0:N){
			arg <- paste(admbDir,"iscam -est -model 8 -nox -retro ",i,sep="")
			system(arg)
			A <- reptoRlist("iscam.rep")
			#std <- read.fit("iscam")$std
			#A$sb.std <- std[length(std)]
			Retro <- c(Retro,list(A))
		}
	}                                      
	arg <- paste(admbDir,"iscam -est -nox",sep="")
	system(arg)
	#Pass the R object to plotting routines
	retro.ssb(Retro,type)
	pt.retro(Retro,type)
	return(Retro)
}

retro.ssb <- function(R,type="wmf"){
	par(mfcol <- c(3,1),lwd=2,cex.axis=1.,cex.lab=1.,mar=c(3,4,1,1))
	nretyears <- length(R)
	n=length(A$yr)
	rsb <- rft <- rrt <- sb.ci <- NULL
	for(i in 1:nretyears){
		sbt <- ft <- rt <- rep(NA,length=n)
		sbt[1:(n-i+1)] <- R[[i]]$sbt[1:(n-i+1)]
		ft[1:(n-i+1)] <- R[[i]]$ft[1:(n-i+1)]
		rt[1:(n-i+1)] <- R[[i]]$nt[1:(n-i+1),1]
		rsb <- cbind(rsb,sbt)
		rft <- cbind(rft,ft)
		rrt <- cbind(rrt,rt)
		sb.ci <<- rbind(sb.ci,c(sbt[n-i+1]-1.97*A$sb.std, sbt[n-i+1]+1.97*A$sb.std))   
		print(c(sbt[n-i+1]-1.97*R[[i]]$sb.std, sbt[n-i+1]+1.97*R[[i]]$sb.std))
	}
	print(sb.ci)
	matplot(A$yr,rsb/2,type="l",ylim=c(0,1.15*max(rsb/2,na.rm=T)),lty=1,lwd=1,col=1:nretyears,xlab="",ylab="SSB (millons mt)")
	legend("topright",paste(max(A$yr):(max(A$yr)-nretyears+1)),lwd=1,col=1:nretyears,bty="n",cex=0.8,ncol=5 )
	matplot(A$yr,rft,type="l",ylim=c(0,max(rft,na.rm=T)),lty=1,lwd=1,col=1:nretyears,xlab="",ylab="Fishing mortality")
	matplot(A$yr,rrt,type="l",ylim=c(0,max(rrt,na.rm=T)),lty=1,lwd=1,col=1:nretyears,xlab="Year",ylab="Age-1 recruits")
	
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigRetroPlots.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=8,height=5)
    }
    else if(type=="wmf"){
      filename <- "iscamFigRetroPlots.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	
	par(mfcol=c(1, 1))
	mcbt <- A$mc.sbt#read.table("iscam.sbt",h=F)
	post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
	btci <- apply(post.bt/2,2,quantile,probs=c(0.025,0.5,0.975))
	matplot(A$yrs,t(btci),type="l",col=1,lty=c(0,1,0), lwd=2,ylim=c(0,max(btci))
		,xlab="Year",ylab="Spawning biomass (million mt)")
	xx <- c(A$yrs, rev(A$yrs)) 
	yy <- c(btci[1, ], rev(btci[3, ]))
	polygon(xx, yy, col="lightgrey", border=F)
	lines(A$yrs,btci[2, ], lwd=2)
	matlines(A$yr, rsb/2,col=1:nretyears)
	legend("topright",paste(max(A$yr):(max(A$yr)-nretyears+1)),lwd=1,col=1:nretyears,bty="n",cex=0.8,ncol=5 )
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigRetroPlots2.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=8,height=5)
    }
    else if(type=="wmf"){
      filename <- "iscamFigRetroPlots2.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
}

pt.retro <- function(R,type="wmf"){
	par(mfcol=c(2,2),lwd=2,cex.axis=1.,cex.lab=1.)
	n <- length(R)
	ryr <- max(A$yr):(max(A$yr)-n+1)
	Q <- SBo <- h <- M <- NULL
	for(i in 1:n){
		Q <- c(Q,R[[i]]$q)
		M <- c(M,R[[i]]$M)
		SBo <- c(SBo,R[[i]]$sbt[1]/2)#R[[i]]$sbt[1]/2)
		h <- c(h,R[[i]]$steepness)
	}
	
	#Q=c(R7$q,R6$q,R5$q,R4$q,R3$q,R2$q,R1$q,R0$q)
	#M=c(R7$M,R6$M,R5$M,R4$M,R3$M,R2$M,R1$M,R0$M)
	#Ro=c(R7$Ro,R6$Ro,R5$Ro,R4$Ro,R3$Ro,R2$Ro,R1$Ro,R0$Ro)
	#h=c(R7$steepness,R6$steepness,R5$steepness,R4$steepness,R3$steepness,R2$steepness,R1$steepness,R0$steepness)
	plot(ryr,Q,ylim=c(0,1),xlab="Retrospective year",ylab="Survey Q"); gletter(1)
	plot(ryr,h,ylim=c(0.2,1),xlab="Retrospective year",ylab="Steepness (h)"); gletter(2)
	plot(ryr,SBo,xlab="Retrospective year",ylab="Unfished female spawning biomass (SBo)",ylim=c(1.27,4)); gletter(3) #,ylim=c(1.27,4)
	plot(ryr,M,xlab="Retrospective year",ylab="M",ylim=c(0.2,0.4));gletter(4)
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamRetroPar.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=10,height=8)
    }
    else if(type=="wmf"){
      filename <- "iscamRetroPar.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
}

plot.sbt2 <- function(admbDir="",type="wmf"){
	op=par(no.readonly=T)
	par(mfcol=c(1,1),mar=c(4,5,1,1),cex=1.75)
  fn <- paste(admbDir,"iscam.sbt",sep="")
	mcbt <- read.table(fn,h=F)
	post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
	btci <- apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975))
	matplot(c(A$yr,2009),t(btci),type="l",col=1,lty=c(3,1,3),ylim=c(0,max(btci))
		,xlab="Year",ylab="Spawning biomass (million mt)")
	#matlines(A$yrs,t(apply(post.bt,2,quantile,probs=c(0.05,0.5,0.95))))
	lines(A$yrs,A$sbt,lwd=2); 
	legend("topright",c("mode","median","95% CI"),lty=c(1,1,3),lwd=c(2,1,1),bty="n")
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigSBT.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamFigSBT.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	
	btci <- apply(post.bt/post.bt[,1],2,quantile,probs=c(0.025,0.5,0.975))
	matplot(c(A$yr,2009),t(btci/btci[,1]),type="l",col=1,lty=c(3,1,3),ylim=c(0,max(btci/btci[,1]))
		,xlab="Year",ylab="Spawning depletion")
	lines(A$yrs,A$sbt/A$sbt[1],lwd=2); 
	abline(h=c(0.25,0.4),lty=c(1,2),col="red",lwd=2)
	legend("topright",c("mode","median","95% CI"),lty=c(1,1,3),lwd=c(2,1,1),bty="n")
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigDT.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamFigDT.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	
  fn <- paste(admbDir,"iscam.ft",sep="")
	mcbt <- read.table(fn,h=F)
	post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
	btci <- apply(post.bt,2,quantile,probs=c(0.1,0.5,0.9))
	matplot(c(A$yr),t(btci),type="l",col=1,lty=c(3,1,3),ylim=c(0,max(btci))
		,xlab="Year",ylab="Fishing mortality")
	#matlines(A$yrs,t(apply(post.bt,2,quantile,probs=c(0.05,0.5,0.95))))
	lines(A$yr,A$ft,lwd=2); 
	legend("topleft",c("mode","median","80% CI"),lty=c(1,1,3),lwd=c(2,1,1),bty="n")
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigFT.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamFigFT.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	
  fn <- paste(admbDir,"iscam.bt",sep="")
	mcbt <- read.table(fn,h=F) 
	post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
	btci <- apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975))
	print(tail(t(btci), 10))
	
	mcbt <- (A$ct/1e6)/mcbt[,1:43]
	post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
	btci <- apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975))
   
	matplot(c(A$yr),t(btci),type="l",col=1,lty=c(3,1,3),ylim=c(0,max(btci))
		,xlab="Year",ylab="Exploitation rate\n(catch/vulnerable biomass)")
	#matlines(A$yrs,t(apply(post.bt,2,quantile,probs=c(0.05,0.5,0.95))))
	lines(A$yr,A$ct/1e6/A$bt[1:43],lwd=2); 
	legend("topleft",c("mode","median","95% CI"),lty=c(1,1,3),lwd=c(2,1,1),bty="n")
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigUT.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamFigUT.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	
  fn <- paste(admbDir,"iscam.rt",sep="")
	mc <- read.table(fn)
	mc.rt <- as.data.frame(window(mcmc(mc),start=Burn,thin=Thin))
	rt <- apply(mc.rt,2,quantile,probs=c(0.025,0.5,0.975))
	xp <- barplot(rt[2,],ylim=c(0,max(rt)),names.arg=A$yr,xlab="Year",ylab="Age-1 recruits (billions)")
	arrows(xp,rt[1,],xp,rt[3,],code=3,angle=90,length=0.01)
	points(xp,A$nt[,1],pch=19,cex=1)
	abline(h=median(as.matrix(mc.rt)),col=2)
	abline(h=mean(as.matrix(mc.rt)),col=3,lty=2)
	legend("topright",c("mode","median","95% CI"),lty=c(-1,1,1),pch=c(19,-1,-1),lwd=c(1,8,1),col=c(1,"grey",1),bty="n")
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigRT.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamFigRT.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################

}

plot.bt <- function(amdbDir="",type="wmf"){
	op <- par(no.readonly=T)
	par(mfcol=c(2,1),mar=c(4,4,1,1))
  fn <- paste(amdbDir,"iscam.sbt",sep="")
	mcbt <- read.table(fn,h=F)
	post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
	post.bt <- post.bt/2
	btci <- apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975))
	matplot(yrs,t(btci),type="l",col=1,lty=c(3,1,3),ylim=c(0,max(btci))
		,xlab="Year",ylab="Spawning biomass (million mt)")
	#matlines(A$yrs,t(apply(post.bt,2,quantile,probs=c(0.05,0.5,0.95))))
	lines(A$yrs,A$sbt/2,lwd=2); gletter(1)
	
	fn <- paste(amdbDir,"iscam.summary.dat",sep="")
	mcsum<- read.table(fn,h=T)
	sbo<-mcsum$Sbo
	sb40sb<-mcsum$SB40.SB
	sb40y<-mcsum$SB40.Y
	
	#mcbt=read.table("iscam.sbt",h=F)
	#post.bt=as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
	btci <- apply(post.bt/sbo,2,quantile,probs=c(0.025,0.5,0.975))
	matplot(yrs,t(btci),type="l",col=1,lty=c(3,1,3),ylim=c(0,max(btci/btci[,1]))
		,xlab="Year",ylab="Spawning depletion")
	#lines(A$yrs,A$sbt/A$sbt[1],lwd=2); 
	abline(h=c(0.25,0.4),lty=c(1,2),col="red",lwd=2)
	gletter(2)
	
	sbci<-quantile(sbo,probs=c(0.025,0.5,0.975))
	sb40ci<-quantile(sb40sb,probs=c(0.025,0.5,0.975))
	sbyci<-quantile(sb40y,probs=c(0.025,0.5,0.975))
	print("sbo quantiles, sb40sb quantiles, sb40y quantiles")
	print(sbci)
	print(sb40ci)
	print(sbyci)
	
	break
	#matlines(rt[,1],rt[,-1]*2,col="grey")
	#d=btci/btci[,1]
	#matplot(c(A$yr,2008),t(d),type="l",lty=c(3,1,3),col=1,ylim=c(0,max(d)))
	
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFig4.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamFig4.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################

	##Table for exec summary
	#mcft=read.table("iscam.ft",h=F)
	
	#iix=c(1,35:42)
	#btci=round(apply(mcbt[,iix],2,quantile,probs=c(0.5,0.05,0.95)),3)
	#ftci=round(apply(mcft[,iix],2,quantile,probs=c(0.5,0.05,0.95)),3)
	#d=round(btci/btci[,1],3)
	#xx=cbind(A$yr[iix],t(btci),t(d),t(ftci))
	#print(cbind(t(btci),t(d),t(ftci)))
	#latex(xx,file=paste(wdir,"TableExec.tex",sep=""),rowname=NULL)
	
	rm(mcbt)
	par(op)
}

plot.lage <- function(){
  #X-axis is year
	#Y-axis is mean length at age.
	par(mfcol=c(1,1))
	plot(A$yr,A$yr,ylim=c(0,1.6),type="n",xlab="Year",ylab="Mean weight-at-age (kg)")
	a <- 6.5359e-6
	b <- 2.98684
	for(i in 1:40)	{
		iy <- 1965+i
		#x=iy:(iy+14)
		y <- a*diag(A$la[0:-i,-1])^b
		y2 <- diag(A$cwa[0:-i,-1])
		x <- 1:length(y)+1965+i
		#lines(x,y,col=i)
		x <- 1:length(y2)+1965+i
		
		lines(x,y2,col=i)
		#points(x[1],y2[1],pch=i,col=i)
		text(x[1],y2[1],x[1]-2,cex=0.5,col=i)
		n <- length(y2)
		text(x[n],y2[n],x[1]-2,cex=0.5,col=i)
	}
}

plot.cage <- function(){
	par(mfrow=c(2,3),mar=c(3,0,2,0),oma=c(3,5,1,2))
	iy <- unique(A$Qdata[,1])
	for(i in iy){
		tmp <- subset(A$Qdata,A$Qdata[,1]==i)
		tla <- subset(A$la,A$la[,1]==i)
		bubble.plot(2:15,tmp[,2],z=t(tmp[,3:16]),scale=0.2,fill=F,log.scale=F,main="Observed",xlab="",ylab="",ylim=c(20,70))
		lines(2:15,tla[3:16])
		
		usr <- par("usr"); inset.x=0.1*(usr[2]-usr[1]); inset.y=0.05*(usr[4]-usr[3])
				text(usr[1]+inset.x,usr[4]-inset.y,paste(i,sep=""),cex=1.5,font=2)
				
		bubble.plot(2:15,tmp[,2],z=t(tmp[,17:30]),scale=0.2,fill=F,log.scale=F,yaxt="n",main="Predicted",xlab="",ylab="",ylim=c(20,70))
		lines(2:15,tla[3:16])
		
		bubble.plot(2:15,tmp[,2],z=t(tmp[,31:44]),scale=0.2,yaxt="n",main="Pearson residual",xlab="",ylab="",ylim=c(20,70))
		#contour(2:15,tmp[,2],t(tmp[,17:30])/max(tmp[,17:30]),add=F,draw=F)
		
		legend("bottom",paste("Absolute max =",max(abs(tmp[,31:44]),na.rm=T)),bty="n")
		mtext("Age",1,outer=T)
		mtext("Length (cm)",2,outer=T,las=0,line=3)
	}
}


#######################################################
## Marginal posteriors  -- this function exists in iscamfigs.r
#######################################################
#plot.marg<-function(xx,breaks=30,...)
#{	#xx is a list(p=samples, mu=prior mean, s=prior varian, fn=prior distribution)
#	ss<-hist(xx$p,prob=T,breaks=breaks,main="",xlab=xx$nm,...)
#	if(!is.null(xx$fn))
#	{
#		xl=ss$breaks
#		pd=xx$fn(xl,xx$mu,xx$sig)
#		lines(xl,pd,col=2)
#	}
#}

post.samp	<- window(mcmc(A$mc[1:length(A$mc[,1]),]),start=Burn,thin=Thin)
colnames(post.samp) <- names(A$mc)
nm	<- names(A$mc)
#Prior distributions - defined in control file (output to report file as A$ctrl)
pdstd=c(dunif,dnorm,dlnorm,dbeta,dgamma)
pdstr=c(runif,rnorm,rlnorm,rbeta,rgamma)
mu <- A$ctrl[,6]
sig <-  A$ctrl[,7]
idst<- A$ctrl[,5]
plot.priors<-function(admbDir="",type="wmf")
{	
	breaks="sturges"
	np=length(mu)
	op<-par(mfrow=c(3,3),mar=c(4,4,1,1))
	
	for(i in 1:np) {
		print(idst[i])
		xx <- list(mu=mu[i],sig=sig[i],fnr=pdstr[[idst[i]+1]],nm=nm[i])
		pd <- xx$fnr(50000,xx$mu,xx$sig)
		ss <- hist(pd,prob=T,breaks=breaks,main="",xlab=xx$nm, col="wheat", xlim=c(min(pd), 1.1*max(pd)), ylim=c(0,1.1*max(density(pd)$y)))
		xl <- seq(min(ss$breaks),max(ss$breaks),length=150)
		lines(density(pd),col=1,lwd=1) 
	}
	 if(saveon){
	    if(type=="eps"){
	      filename="iscamPriors.eps"
	      dev.copy2eps(file=paste(figdir,filename,sep=""))
	    }else if(type=="wmf"){
	      filename <- "iscamPriors.wmf"
	      filename <- paste(figdir,filename,sep="")
	      savePlot(filename,type="wmf")
	    }
  }
  par(op)
}


plot.rt <- function(admbDir="",type="wmf"){
	op <- par(no.readonly=T)
	par(mfcol=c(1,1))
  fn <- paste(admbDir,"iscam.rt",sep="")
	mc <- read.table(fn)
	mc.rt <- as.data.frame(window(mcmc(mc),start=Burn,thin=Thin))
	rt <- apply(mc.rt,2,quantile,probs=c(0.025,0.5,0.975))
	xp <- barplot(rt[2,],ylim=c(0,max(rt)),names.arg=A$yr,xlab="Year",ylab="Age-1 recruits (billions)")
	arrows(xp,rt[1,],xp,rt[3,],code=3,angle=90,length=0.01)
	points(xp,A$nt[,1],pch=19,cex=1)
	abline(h=median(as.matrix(mc.rt)),col=2)
	abline(h=mean(as.matrix(mc.rt)),col=3,lty=2)
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigRt.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamFigRt.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	par(op)
}

plot.pars <- function(admbDir="",type="wmf"){	
	op <- par(no.readonly=T)
	require(MCMCpack)
  fn <- paste(admbDir,"iscam.mcmc",sep="")
	mc=read.table(fn,header=T)
  fn <- paste(admbDir,"iscam.sbt",sep="")
	sb <- read.table(fn,header=F)
	mc[,9] <- 1/mc[,9]
	par(mfcol=c(3,3),las=1,mar=c(5,4,1,1))
	mu <- c(log(0.2),log(0.35),log(0.23),0,0.05,0,0.05,3.5,7.498)
	sig <- c(0.396,0.262,0.1,14,5.0,14,5.0,31.5,5.7835)
	nm <- names(mc)
	fn <- c(dlnorm,dlnorm,dlnorm,dunif,dunif,dunif,dunif,dbeta,dinvgamma)
	ptable <- NULL
  fn <- paste(admbDir,"iscam.std",sep="")
	mle <- read.table(fn,skip=1,nrow=10,fill=T)
	mle <- mle[2:10,3:4]; #print(mle)
	for(i in 1:9){		
		xx <- list(p=mc[,i],mu=mu[i],sig=sig[i],fn=fn[[i]],nm=nm[i])
		plot.marg(xx)
		ptable <- rbind(ptable,c(nm[i],round(quantile(mc[,i],probs=c(0.5,0.05,0.95)),3)))
	}
	ptable <- rbind(ptable,c("q",round(quantile(mc$q,probs=c(0.5,0.05,0.95)),3)))  #make a table of this.
	ptable <- rbind(ptable,c("h",round(quantile(mc$h,probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("SSB depletion",round(quantile(mc$depletion,probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("abc",round(quantile(mc$abc,probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("Bo",round(quantile(mc$Bo,probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("Bo3",round(quantile(mc$Bo3,probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("sbmsy",round(quantile(mc$sbmsy,probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("sbo",round(quantile(sb[,1],probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("sb2007",round(quantile(sb[,42],probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("female sbmsy",round(quantile(mc$sbmsy/2,probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("female sbo",round(quantile(sb[,1]/2,probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("female sb2007",round(quantile(sb[,42]/2,probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("sig",round(quantile(sqrt(mc$rho*1/mc$varphi),probs=c(0.5,0.05,0.95)),3)))
	ptable <- rbind(ptable,c("tau",round(quantile(sqrt((1-mc$rho)*1/mc$varphi),probs=c(0.5,0.05,0.95)),3)))
	print(ptable)
	#latex(ptable,file=paste(wdir,"iscamParMedian.tex",sep=""),rowname=NULL)
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigMarginals.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamFigMarginals.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	par(op)
}

table.reference.points <- function(admbDir=""){
	get.stats <- function(mcx){
		round(quantile(mcx,probs=c(0.5,0.025,0.975)),3)
	}
  fn <- paste(admbDir,"iscam.mcmc",sep="")
	mc <- read.table(fn,header=T)
	idx <- c(16,18,19)
	ptable <- apply(mc[,idx],2,get.stats)
	print(t(ptable))
}

plot.sb <- function(admbDir=""){
	require(MCMCpack)
  fn <- paste(admbDir,"iscam.mcmc",sep="")
	mc <- read.table(fn,header=T)
	mc[,9] <- 1/mc[,9]
	par(mfcol=c(3,2),mar=c(5,4,1,1))
  fn <- paste(admbDir,"iscam.sbt",sep="")
	mm <- read.table(fn)
	sbo <- mm[,1]
	sbn <- mm[,dim(mm)[2]]
	sbmsy <- mc$sbmsy
	abc <- mc$abc
	dep <- mc$depletion
	xx <- list(p=sbo,mu=0,sig=0,fn=NULL,nm="SBo"); plot.marg(xx,breaks=50); gletter(1)
	xx <- list(p=sbmsy,mu=0,sig=0,fn=NULL,nm="SBmsy"); plot.marg(xx); gletter(2)
	xx <- list(p=sbmsy/sbo,mu=0,sig=0,fn=NULL,nm="SBmsy/SBo"); plot.marg(xx); gletter(3)
	xx <- list(p=sbn/sbmsy,mu=0,sig=0,fn=NULL,nm="SB2007/SBmsy"); plot.marg(xx); gletter(4)
	xx <- list(p=dep,mu=0,sig=0,fn=NULL,nm="SB2007/SBo"); plot.marg(xx); gletter(5)
	xx <- list(p=abc,mu=0,sig=0,fn=NULL,nm="ABC"); plot.marg(xx,breaks=50,col="red"); gletter(6)
	
}

plot.res <- function(admbDir="",type="wmf")
{
	op <- par(no.readonly=T)
  	mc <-A$mc.resid
	mc.res <- as.data.frame(window(mcmc(mc),start=Burn,thin=Thin))
	boxplot(mc.res,names=A$iyr,pch="-",xlab="Year",ylab="Acoustic biomass survey residual")
	abline(h=0)

	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigRes.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamFigRes.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }

	par(op)
}

plot.SR <- function(admbDir="",type="wmf")
{
	op <- par(no.readonly=T)
	
	#SR PLOT
	alph=A$so
	bet=A$bta
	SB=A$sbt[1:nyr]
	Rec=A$N[1:nyr,1]
	
	plot(SB,Rec, pch=20, xlab="Spawning biomass (million mt)", ylab="Recruits (billion)", xlim=c(0,1.2*max(SB)), las=1)
	sblist=seq(0,1.2*max(SB), by=0.1)
	SR=(alph*sblist)/(1+bet*sblist)
	lines(sblist,SR)

	if(saveon){
   	 if(type=="eps"){
      filename <- "SRplot.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "SRplot.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }

	par(op)
}


plot.cumbt <- function(admbDir="",type="wmf"){
	op <- par(no.readonly=T)
	par(mfcol=c(1,2),las=1)
  fn <- paste(admbDir,"iscam.nt",sep="")
	mm <- read.table(fn,head=F)
	xx <- t(apply(mm,1,cumsum));
	xx <- xx#/xx[,15]
	x1 <- apply(xx,2,quantile,prob=c(0.025,0.5,0.975))
	x2 <- apply(xx/xx[,15],2,quantile,prob=c(0.025,0.5,0.975))
	matplot(t(x1),type="l",lty=c(2,1,2),col=1,xlab="Age",ylab="Cumulative spawning biomass"); gletter(1)
	matplot(t(x2),type="l",lty=c(2,1,2),col=1,xlab="Age",ylab="Cumulative spawning biomass\n(% of total)"); gletter(2)
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamFigCumBt.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamFigCumBt.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	par(op)
}

########################################################
## Risk plots
########################################################
risk2.plot <- function(type="wmf"){
	par(mfcol=c(1,1),cex.axis=1.,cex.lab=1.,mar=c(5,5,1,1))
	fn <- function(P,add=F,...)	{
		attach(P)
		ff <- Fstatus
    ff[ff<=1] <- 0
    ff[ff!=0] <- 1
		fit.f <- glm(ff~ABC,family=binomial(logit))	
		if(add){
      plot(jitter(ABC),ff,xlab="ABC option",ylab="P(Ft > Fmsy)",pch=".",...)
    }
		lines(sort(ABC),sort(fit.f$fitted.values),lwd=1,...)
		detach(P)
	}
	fn(P1,add=T,col=1)
	fn(P2,col=2)
	fn(P3,col=3)
	
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamR4_4.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamR4_4b.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	fn(P4,col=4)

	plot.ecdf(mcbase$abc,xlab="ABC",col.vert=1,col.hor=1,add=T,do.points=F,verticals=T,lwd=2)
	plot.ecdf(mchighf$abc,xlab="ABC",col.vert=2,col.hor=2,add=T,do.points=F,verticals=T,lwd=2)
	plot.ecdf(mclowf$abc,xlab="ABC",col.vert=3,col.hor=3,add=T,do.points=F,verticals=T,lwd=2)
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamR4_5.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamR4_5.wmf"
      filename <- paste(fidir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
	plot.ecdf(mc.comp$abc,xlab="ABC",col.vert=4,col.hor=4,add=T,do.points=F,verticals=T,lwd=2)
	## FIG	#######################################
	if(saveon){
    if(type=="eps"){
      filename <- "iscamR4.5b.eps"
      dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
    }
    else if(type=="wmf"){
      filename <- "iscamR4.5b.wmf"
      filename <- paste(figdir,filename,sep="")
      savePlot(filename,type="wmf")
    }
  }
	#############################################
}

risk.plot <-function(admbDir="",type="wmf"){
	op <- par(no.readonly=T)
  fn <- paste(admbDir,"iscam.proj",sep="")
	P <- read.table(fn,header=T,sep="\t",fill=T,nrow=5000)
	P <- P[P$Fstatus>0,]
	ix <- sample(1:dim(P)[1],1000,replace=F)
	
	with(P,{
		ABC <- jitter(ABC,1.5)
		ff <- Fstatus
    ff[ff<=1] <- 0
    ff[ff!=0] <- 1
		sb <- SBstatus
    sb[sb>=1] <- 0
    sb[sb!=0] <- 1
		sl <- SBlimit
    sl[sl>=1] <- 0
    sl[sl!=0] <- 1
		s40 <- SB40
    s40[sl>=1] <- 0
    s40[s40!=0] <- 1
		fit.f <- glm(ff~ABC,family=binomial(logit))
		fit.s <- glm(sb~ABC,family=binomial(logit))
		fit.l <- glm(sl~ABC,family=binomial(logit))
		fit.40 <- glm(s40~ABC,family=binomial(logit))

		par(mfcol=c(2,2))
		plot(ABC[ix],ff[ix],xlab="ABC option",ylab="P(Ft > Fmsy)",pch=".")
		lines(sort(ABC[ix]),sort(fit.f$fitted.values[ix]),lwd=1,col="red")
		gletter(1)

		plot(ABC[ix],sb[ix],xlab="ABC option",ylab="P(Sbt+1 < Sbt)",pch=".")
		lines(sort(ABC[ix]),sort(fit.s$fitted.values[ix]),lwd=1,col="red")
		gletter(2)

		plot(ABC[ix],sl[ix],xlab="ABC option",ylab="P(Sbt+1 < Sbmsy)",pch=".")
		lines(sort(ABC[ix]),sort(fit.l$fitted.values[ix]),lwd=1,col="red")
		gletter(3)

		plot(ABC[ix],s40[ix],xlab="ABC option",ylab="P(Sbt+1 < Sb40)",pch=".")
		lines(sort(ABC[ix]),sort(fit.40$fitted.values[ix]),lwd=1,col="red")
		gletter(4)
    ## FIG	#######################################
    if(saveon){
      if(type=="eps"){
        filename <- "iscamFigRisk.eps"
        dev.copy2eps(file=paste(figdir,filename,sep=""), width=9,height=7)
      }
      else if(type=="wmf"){
        filename <- "iscamFigRisk.wmf"
        filename <- paste(figdir,filename,sep="")
        savePlot(filename,type="wmf")
      }
    }
		#############################################
		fo <- coef(fit.f); so=coef(fit.s); lo=coef(fit.l); f4=coef(fit.40);
		probs <- seq(0.05,0.95,by=0.05)
		lp <- log(probs/(1-probs))
		tac <- round((lp-fo[1])/fo[2],3); tac[tac<0]=0
		stac <- round((lp-so[1])/so[2],2); stac[stac<0]=0
		tac40 <- round((lp-f4[1])/f4[2],2); tac40[tac40<0]=0

		TACtable <- cbind(probs,tac,stac,tac40)
		print(cbind(probs,tac,stac,tac40))
		filename <- paste(tabdir,"TableTACadem.tex",sep="")
		cap <- "Probability of exceeding management targets for a give ABC option"
		latex(TACtable,file=filename,rowname=NULL,caption=cap)
	})
	par(op)
}

##############################################################
## Tables for data.
##############################################################
old.table.3 <- function(admbDir=""){
  #Table with catch, mean age and survey abundance index
	#may want to add mean age of the survey comps.
	x <- data.frame(Year=A$yr,Ct=A$ct)
	ii <- A$yr%in%A$ayr
	mu.age <- rep(NA,length(A$yr))
	mu.age[ii] <- 2:15%*%t(A$A)
	x <- data.frame(x,muage=round(mu.age,1))
	
	ii <- A$yr%in%A$iyr
	yt <- rep(NA,length(A$yr))
	yt[ii] <- A$yt
	x <- data.frame(x,It=yt)
	Table3 <- cbind(x[1:21,],x[22:42,])
	cap <- "Combined historical landings (mt) for the U.S. and Can. fisheries, 
	mean age of the catch, and survey abundance indices (millions mt) from 
	the acoustic-trawl survey."
	filename <- paste(tabdir,"Table3.tex",sep="")
	#latex(Table3,file=filename,rowname=NULL,caption=cap)
	
}

old.table.4 <- function(admbDir=""){
  #proportion at age data used in the assessment.
	aa <- round(A$A*100,2)
  colnames(aa) <- paste(2:15)
	x <- data.frame(Year=A$ayr,age=aa) 
	cap <- "Age-composition (reported in percentages) of the combined U.S. and Can. commercial catch from 1977-2007. Age-15 represents a plus group."
	filename <- paste(wdir,"Junk.tex",sep="")
	latex(x,file=filename,rowname=NULL,caption=cap,landscape=T)
}

old.table.5 <- function(admbDir=""){
  #Some sort of table to summarize the survey proportions at age.
	iyr <- A$iyr
	fn <- function(i) subset(A$yAt,A$yAt[,1]==i)
	yAt <- sapply(iyr,fn)	#yAt is now a list for each survey year.
	
	page <- function(M)colSums(M[,6:19])
	x <- t(sapply(yAt,page))
	colnames(x)=paste(2:15)
	bubble.plot(iyr,2:15,x,scale=0.4)
	dd <- data.frame(Year=iyr,age=round(x*100,2))
	cap <- "Age-composition (percent) from acoustic surveys from 1977-2007.  
	Note that these data are the collapsed conditional age-length data 
	multiplied by the length frequencies and collapsed over the size intervals 
	(age 15 represents a plus group)."
	filename <- paste(wdir,"Table5.tex",sep="")
	#latex(dd,file=filename,rowname=NULL,caption=cap)
}
