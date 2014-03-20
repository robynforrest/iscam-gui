fig.a <- function(){
	# Total landings
	op <- par(no.readonly=T)
	barplot(A$obs_ct, names.arg=A$yr, xlab="Year", ylab="Landings", las=1)  #must plot observed catch
       saveFig("Catch")
	par(op)
}

fig.effort <- function(){
	# Total effort
	op <- par(no.readonly=T)
	plot(A$yr[1:length(A$effort)], A$effort/1000,  xlab="Year", ylab="Fishing effort (1000 hr)", type="l", lty=1, lwd=2, col="darkblue",main="Effort",las=1)  
  	saveFig("Effort")
	par(op)
}

fig.b <- function(includeMPD=F,ylimit=6,useMaxYlim=T,opacity="20",...){
	if(A$delaydiff==1){	
		ddmcmc <- A$mcproj 
		dmcmc <- subset(ddmcmc, tac==0)	   #take only the first tac from each posterior sample - the control points do not change with tac
		post.dmcmc <- as.data.frame(window(mcmc(dmcmc),start=Burn,thin=Thin))
		Bmin <- post.dmcmc$Bmin
		Bmean <-  post.dmcmc$BAvg_S
		bmin<-median(Bmin)
		bmean<-median(Bmean)
	   	B2014<-	post.dmcmc$B2014/1000
		B2014ci <- quantile(B2014, na.rm=T,c(0.025,0.5,0.975))
	}		
	
	# Spawning stock biomass
	op	<- par(no.readonly=T)
 	mcbo <- (A$mc$bo)/1000
 	post.bo <- as.data.frame(window(mcmc(mcbo),start=Burn,thin=Thin))
	boci <- apply(post.bo,2,quantile,probs=c(0.025,0.5,0.975))

	if(A$delaydiff==1) mcbt <- (A$mc.sbt[,1:nyear])/1000
	if(A$delaydiff==0) mcbt <- (A$mc.sbt[,1:nyrs])/1000 #projection code not written yet for ASM so just use projection year from main model
	post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
	btci <- apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975))
  if(useMaxYlim){
    yUpperLimit <- max(btci,(A$sbt)/1000)
  }else{
    yUpperLimit <- ylimit
  }
 
 if(A$delaydiff==1) btci<-cbind(btci,B2014ci) #add the projection year from the projection model with sampled log anomalies

  matplot(A$yr,t(btci[,1:nyear]),type="l",col=1,lty=c(2,1,2), lwd=2,ylim=c(0,yUpperLimit),ylab="Biomass (thousand t)", xlab="Year", main="Biomass", las=1)
  xx <- c(A$yr,rev(A$yr))
  yy <- c(btci[1,1:nyear],rev(btci[3,1:nyear]))
  shade <- getShade(1,opacity)
  polygon(xx,yy,density=NA,col=shade)
  points(A$yrs[1]-0.8,boci[2],col=1,pch=1)
  arrows(A$yrs[1]-0.8,boci[1],A$yrs[1]-0.8,boci[3],col=1, code=0, lwd=1.5)
  
  if(A$delaydiff==1){
 	 abline(h=bmin/1000,col=2,lty=2)
 	 abline(h=bmean/1000,col=3,lty=2)
   }

 #projection year
  lines(2013:2014,  btci[2,nyear:nyrs], col=2, lwd=2)
  lines(2013:2014,  btci[1,nyear:nyrs], col=2, lwd=2, lty=2)
  lines(2013:2014,  btci[3,nyear:nyrs], col=2, lwd=2, lty=2)
  xxp <- c(2013:2014,rev(2013:2014))
  yyp <- c(btci[1,nyear:nyrs],rev(btci[3,nyear:nyrs]))
  shade <- getShade(2,opacity)
  polygon(xxp,yyp,density=NA,col=shade)
  if(A$delaydiff==1) legend("topright",c("Median Bmin","Median BAvg 1956-2004"),lty=c(2,2),pch=c(-1,-1),lwd=c(1,1),col=c(2,3),bty="n")

  if(includeMPD){
    Bt<-A$sbt 
    lines(A$yrs,Bt/1000,type="l",col=mpdLineColor,lty=1, lwd=2,ylim=c(0,yUpperLimit), las=1, xlab="",ylab="")
    #  	legend("topright",c("MPD estimate"),lty=1,lwd=2,col=mpdLineColor,bty="n")
  }
  saveFig("fig.spawningbiomass")
	par(op)
}

fig.biomass.mpd <- function(ylimit=45,useMaxYlim=T,...){
	# Spawning stock biomass
	op	<- par(no.readonly=T)
	if(A$delaydiff==1){
		ddmpd <- A$mpdproj
		bmin <- ddmpd$Bmin[1]
		bmean <-  ddmpd$BAvg_S[1] }

  if(useMaxYlim){
    yUpperLimit <- max(A$sbt)/1000 #max((A$sbt)/1000)
  }else{
    yUpperLimit <- ylimit
  }
  Bt<-A$sbt 
  
  plot(A$yrs,Bt/1000,type="l",col=mpdLineColor,lty=1, lwd=2,ylim=c(0,yUpperLimit),ylab="Biomass (thousand t)", xlab="Year", main="Biomass", las=1) #for pcod testing - sbo is male + female
  points(A$yr[1]-0.8,(A$sbo)/1000,col=mpdLineColor,pch=1)
  if(A$delaydiff==1){	
  	abline(h=bmin/1000,col=2,lty=2)
  	abline(h=bmean/1000,col=3,lty=2)
  	legend("topright",c("MPD Bmin","MPD BAvg 1956-2004"),lty=c(2,2),pch=c(-1,-1),lwd=c(1,1),col=c(2,3),bty="n")
   }
  
  #mtext("Effective SB0_2005_model = SB0 * brat = 21,078 x 0.675 = 14228 t",side=3, line=0, outer=F)
  saveFig("fig.spawningbiomassMPD")
	par(op)
}

#total biomass
fig.bt <- function(includeMPD=F,ylimit=150,useMaxYlim=T,opacity="20",...){
	# Total stock biomass
	op	<- par(no.readonly=T)
  	mcbt <- (A$mc.tbt)/1000
  	post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
	 btci <- apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975))
  if(useMaxYlim){
    yUpperLimit <- max(btci,(A$tbt)/1000)
  }else{
    yUpperLimit <- ylimit
  }

 matplot(A$yr,t(btci[,1:nyear]),type="l",col=1,lty=c(2,1,2), lwd=2,ylim=c(0,yUpperLimit),ylab="Total biomass (thousand t)", las=1)
  xx <- c(A$yr,rev(A$yr))
  yy <- c(btci[1,1:nyear],rev(btci[3,1:nyear]))
  shade <- getShade(1,opacity)
  polygon(xx,yy,density=NA,col=shade)
  
  if(includeMPD){
    lines(A$yr,(A$tbt[1:nyear])/1000,type="l",col=mpdLineColor,lty=1, lwd=2,ylim=c(0,yUpperLimit), las=1, xlab="",ylab="")
   	legend("topright",c("MPD estimate"),lty=1,lwd=2,col=mpdLineColor,bty="n")
  }
  saveFig("fig.Totalbiomass")
	par(op)
}

fig.bt.mpd <- function(ylimit=150,useMaxYlim=T,...){
	# Total stock biomass
	op	<- par(no.readonly=T)
  if(useMaxYlim){
    yUpperLimit <- max(A$tbt/1000)
  }else{
    yUpperLimit <- ylimit
  }
  
 matplot(A$yr,(A$tbt[,1:nyear])/1000,type="l",col=mpdLineColor,lty=1, lwd=2,ylim=c(0,yUpperLimit),ylab="Total biomass (thousand t)", las=1) 
  abline(h=median(A$tbt[1:nyear])/1000,col=2,lty=2)
  legend("topright","MPD long-term median",lty=2,pch=-1,lwd=1,col=2,bty="n")
   saveFig("fig.TotalbiomassMPD")
	par(op)
}


fig.biomass.recruits <- function(yBiomassYlim=45,
                                 useMaxBiomassYlim=T,
                                 yRecruitmentYlim=250,
                                 useMaxRecruitmentYlim=T){
  # make two-panel plot of biomass and recruitment
	op	<- par(no.readonly=T)	
  par(oma=c(1,1,1,1),mar=c(3,3,1,1))
  par(mfrow=c(2,1))
  fig.b(ylimit=yBiomassYlim,useMaxYlim=useMaxBiomassYlim, xlab="")
  fig.d(ylimit=yRecruitmentYlim,useMaxYlim=useMaxRecruitmentYlim,xlab="Year")
  saveFig("fig.biomass.recruitment")
  par(op)
}

fig.depletion.mpd <- function(ylimit=3.5,useMaxYlim=T){
	# Spawning stock biomass
	op	<- par(no.readonly=T)
  if(useMaxYlim){
    yUpperLimit <- max(A$sbt/A$sbo)
  }else{
    yUpperLimit <- ylimit
  }
  matplot(A$yr,A$sbt[1:nyear]/A$sbo,type="l",col="blue",lty=1, lwd=2,ylim=c(0,yUpperLimit),xlab="Year",ylab="Spawning Depletion",main="Spawning Depletion", las=1)
	abline(h=0.40, lwd=mtLineWidth, col=mtLineColor, lty=mtLineType)
  legend("topright",c("0.4 SB0"),lty=mtLineType,lwd=mtLineWidth,col=mtLineColor,bty="n")
   saveFig("fig.depletionMPD")
	par(op)
}

fig.c <- function(includeMPD=F,ylimit=3.5,useMaxYlim=T,opacity="20", ...){
	# Spawning stock depletion
	op	<- par(no.readonly=T)
	mcdt <- A$mc.sbdepletion
	post.dt  <- as.data.frame(window(mcmc(mcdt),start=Burn,thin=Thin))
	dtci <- apply(post.dt,2,quantile,probs=c(0.025,0.5,0.975))
  if(useMaxYlim){
    yUpperLimit <- max(dtci,A$sbt/A$sbo)
  }else{
    yUpperLimit <- ylimit
  }
	matplot(A$yr,t(dtci[,1:nyear]),type="l",col=1,lty=c(2,1,2), lwd=2,ylim=c(0,yUpperLimit),xlab="Year",las=1,ylab="Spawning depletion",main="Spawning Depletion")
	
  xx <- c(A$yr,rev(A$yr))
  yy <- c(dtci[1,1:nyear],rev(dtci[3,1:nyear]))
  shade <- getShade(1,opacity)
  polygon(xx,yy,density=NA,col=shade)

	abline(h=0.40, lwd=mtLineWidth, col=mtLineColor, lty=mtLineType)
	if(includeMPD){
    lines(A$yr,A$sbt[1:nyear]/A$sbo,type="l",col=mpdLineColor,lty=1, lwd=2,ylim=c(0,yUpperLimit), las=1, xlab="",ylab="")
   	legend("topright",c("0.4 SB0","MPD estimate"),lty=c(mtLineType,1),
           lwd=c(2,2),col=c(mtLineColor,mpdLineColor),bty="n")
  }else{
    legend("topright",c("0.4 SB0"),lty=c(mtLineType),lwd=c(mtLineWidth),col=c(mtLineColor),bty="n")
  }
  saveFig("Depletion")
	par(op)
}

#RECRUITS
#this was figure e in 2010 assessment
fig.d <- function(ylimit=250,useMaxYlim=T, ...){   
	op	<- par(no.readonly=T)
	sage<-A$sage
	ryr <- yr[(1+sage):nyear]
	#ryr<-yr
	
	plot(ryr, A$rt/1000, lty=1, col=1,type="o", pch=19,ylim=c(0,1.2*max(A$rt/1000)), xlab="Year",ylab="Recruits (million)", las=1, main="Recruits")
	abline(h=median(A$rt)/1000,col=2,lty=2)
	abline(h=mean(A$rt)/1000,col=3,lty=2)
	legend("topright",legend=c("MPD long-term median","MPD long-term mean"),lty=c(2,2),pch=c(-1,-1),lwd=c(1,1),col=c(2,3),bty="n")
  	saveFig("fig.RecruitsMPD")
	par(op)
}

#RECRUITS
#this was figure e in 2010 assessment
fig.dmcmc <- function(ylimit=250,useMaxYlim=T, ...){   
	#recruits
	op	<- par(no.readonly=T)
	sage<-A$sage
	ryr <- yr[(1+sage):nyear]
	#ryr <- yr
	
	mc <- A$mc.rt
	mc.rt <- as.data.frame(window(mcmc(mc),start=Burn,thin=Thin)) 
	rt <- apply(mc.rt,2,quantile,probs=c(0.025,0.5,0.975)) #gets quantiles for number of age 1 recruits
	  if(useMaxYlim){
	    yUpperLimit <- max(rt)/1000 #
	  }else{
	    yUpperLimit <- ylimit
	  }
	xp <- plot(ryr, rt[2,]/1000, type="p", pch=20,ylim=c(0,yUpperLimit), xlab="Year",ylab="Recruits (million)",main="Recruits", las=1) #divide by 1000 to get millions (assume input catch in tonnes and body weight in kg)
	arrows(ryr, rt[1, ]/1000,ryr,rt[3,]/1000,code=3,angle=90,length=0.01)
	
	##points(xp,A$nt[,1],pch=19,cex=1)
	abline(h=median(as.matrix(mc.rt))/1000,col=2, lty=2)
	abline(h=mean(as.matrix(mc.rt))/1000,col=3,lty=2)
	legend("topright",c("MCMC long-term median","MCMC long-term mean"),lty=c(2,2),pch=c(-1,-1),lwd=c(1,1),col=c(2,3),bty="n")
	 saveFig("fig.RecruitsMCMC")
 
	par(op)
}

#SPR status - fmsy
fig.e1 <- function(){
	#The relative spawning potential ratio (1-spr)/(1-spr.at.msy)  
	op	<- par(no.readonly=T)
	spr <- A$mc.sprstatus_fmsy #read.table("ccam.spr",h=F)
	post.spr <- as.data.frame(window(mcmc(spr),start=Burn,thin=Thin))
	sprci <- apply(post.spr,2,quantile,probs=c(0.025,0.5,0.975))
	
	matplot(A$yr,t(sprci),type="l",col=c(2,1,2),lty=c(3,1,3), lwd=2, pch=c(-1, 0, 1),ylim=c(0,2)
		,xlab="Year",ylab="(1-SPR)/(1-SPR at fmsy)")
	abline(h=1, lwd=mtLineWidth, col=mtLineColor, lty=mtLineType)
	text(1980, 1, "Management target", pos=3)
	
  saveFig("fige_fmsy")
	par(op)
}

#SPR status - f40
fig.e2 <- function(){
	#The relative spawning potential ratio (1-spr)/(1-spr.at.msy)  
	op	<- par(no.readonly=T)
	spr <- A$mc.sprstatus_f40 #read.table("ccam.spr",h=F)
	post.spr <- as.data.frame(window(mcmc(spr),start=Burn,thin=Thin))
	sprci <- apply(post.spr,2,quantile,probs=c(0.025,0.5,0.975))
	
	matplot(A$yr,t(sprci),type="l",col=c(2,1,2),lty=c(3,1,3), lwd=2, pch=c(-1, 0, 1),ylim=c(0,2)
		,xlab="Year",ylab="(1-SPR)/(1-SPR at f40)")
	abline(h=1, lwd=mtLineWidth, col=mtLineColor, lty=mtLineType)
	text(1980, 1, "Management target", pos=3)
  saveFig("fige_f40")
	par(op)
}

#FISHING MORTALITY   MCMC
fig.Fmcmc <- function(opacity="20",...){
	ddmcmc <- A$mcproj 
	dmcmc <- subset(ddmcmc, tac==0)	   #take only the first tac from each posterior sample - the control points do not change with tac
	post.dmcmc <- as.data.frame(window(mcmc(dmcmc),start=Burn,thin=Thin))
	Fmean <-  post.dmcmc$FAvg_S
	Fmean<-median(Fmean)
	
	
	#Fishing mortality - GEAR 1 ONLY
	op	<- par(no.readonly=T) 
	ft <- A$mc.ft #read.table("ccam.bt2",h=F)
	post.ft <- as.data.frame(window(mcmc(ft),start=Burn,thin=Thin))
	ftci <- apply(post.ft,2,quantile,probs=c(0.025,0.5,0.975))
		
	matplot(A$yr,t(ftci[, 1:length(A$yr)]),type="l",col=1,lty=c(2,1,2), lwd=2, pch=c(-1, 0, 1)
		,xlab="Year",ylab="Fishing mortality rate (/yr)",main="Fishing mortality", ylim=c(0,1.1*max(ftci)))#ylim=c(0,   1.1*max(ftci)
	xx <- c(A$yr,rev(A$yr))
	  yy <- c(ftci[1,],rev(ftci[3,]))
	  shade <- getShade(1,opacity)
 	 polygon(xx,yy,density=NA,col=shade)
	
	#abline(h=median(as.matrix(ft)),col=3,lty=2)
	abline(h=Fmean,col=2,lty=2)
   	legend("topleft",c("MCMC Fishing mortality", "Median FAvg 1956-2004"),lty=c(1,2),lwd=c(2,1),col=c(1, 2),bty="n")

       saveFig("fig.FishingMortalityMCMC")
	par(op)
  }

#FISHING MORTALITY   MPD
fig.Fmpd <- function(opacity="20",...){
	#Fishing mortality - GEAR 1 ONLY
	op	<- par(no.readonly=T) 
			
	plot(A$yr, A$ft[1,1:length(A$yr)],type="l",col=mpdLineColor,lty=1, lwd=2
		,xlab="Year",ylab="Fishing mortality rate (/yr)",main="Fishing mortality", ylim=c(0,1.1*max(A$ft)))#ylim=c(0,
		
	lines(A$yr, A$ut[1:length(A$yr)], col="darkmagenta",lty=1, lwd=2)
	abline(h=median(A$ft[1,]),col=3,lty=2)
	abline(h=mean(A$ft[1,]),col=2,lty=2)
	abline(h=max(A$ft),col=mpdLineColor,lty=3)
	abline(h=max(A$ut),col="darkmagenta",lty=3)
		
   	legend("topleft",c("MPD Fishing mortality","Exploitation Rate MPD", "MPD median fishing mortality","MPD mean fishing mortality","MPD max fishing mortality","MPD max exploitation rate"),lty=c(1,1,2,2,3,3),lwd=c(rep(2,2),rep(1,4)),col=c(mpdLineColor, 2,3,2,mpdLineColor,"darkmagenta"),bty="n")
   
   	
      saveFig("fig.FishingMortalityMPD")
	par(op)
}

fig.h	<-	function(){
	op <- par(no.readonly=T) 
	par(mfcol=c(1,2))
	n <- length(A$yr)
	
	ft <- A$mc.ft #read.table("ccam.ft",h=F)
	sbt <- A$mc.sbt #read.table("ccam.sbt",h=F)
	mc <- A$mc #read.table("ccam.mcmc", h=T)
	sbstatus <- sbt/mc$bmsy
	fstatus <- ft/mc$fmsy
	
	sbci <- apply(sbstatus, 2, quantile, probs=c(0.5))
	ftci <- apply(fstatus, 2, quantile, probs=c(0.5))
	plot(sbci[1:n], ftci[1:n], type="n",xlim=c(0,1.2*max(sbci[1:n])), ylim=c(0,1.2*max(ftci)), xlab="Median Bt/Bmsy", ylab="Median Ft/Fmsy")
	abline(h=1,v=1,lty=2,col=2)
	lines(sbci[1:n], ftci[1:n],type="o")
	gletter(1)
	
	#2nd plot
	sbstatus <- sbt/mc$bo
	sbci <- apply(sbstatus, 2, quantile, probs=c(0.5))
	ftci <- apply(ft, 2, quantile, probs=c(0.5))
	ssb <- seq(0,2,length=100)
	fp <- median(mc$fmsy)*(ssb-0.1)/0.3
	fp[ssb<=0.1] <- 0
	fp[ssb>0.4] <- median(mc$fmsy)
 	maxY <- max(fp,A$ft)*1.1
	plot(ssb,fp,type="l",ylim=c(0,maxY),lwd=2,xlab="Median SBt/SBo",ylab="Median Ft")
	lines(ssb, fp, lwd=2)
	lines(sbci[1:n],ftci, type="o")
	gletter(2)

  saveFig("KobePlot")
	par(op)
}

fig.i <- function(){
	#plot the equilibrium yield curves  
	op <- par(no.readonly=T)
	par(mfcol=c(2,2))
	#A$equil comes from the ccam.rep file
	fe <- A$equil[, 2]
	ye <- A$equil[, 3]
	sde <- A$equil[, 5]
	spr <- A$equil[, 7]

	plot(fe, ye, type="l", xlab="Fishing mortality (Fe)", ylab="Equilibrium yield")
  gletter(1)

  plot(sde, ye, type="l", xlab="Spawning depletion", ylab="Equilibrium yield", lty=2, col=2)
  gletter(2)

	plot(spr,ye, type="l", xlab="Spawning potential ratio", ylab="Equilibrium yield", lty=3, col=3)
  gletter(3)

	matplot(cbind(fe, sde, spr), ye/max(ye)*100, type="l",xlab="Fe, depletion,  SPR",  ylab="Relative equilibrium yield")
  gletter(4)
	
  saveFig("EqmYield")
	par(op)
}

fig.j <- function(){
	#Relationship between fishing mortlaity ~ yield,  recruitment,  SBe,  SPR
	op <- par(no.readonly=T)
	par(mfcol=c(2,2))
	
	fe <- A$equil[, 2]
	ye <- A$equil[, 3]
	sde <- A$equil[, 5]
	re <- A$equil[, 6] 
	spr <- A$equil[, 7]
	
	
	ix <- c(min(which(ye==max(ye))),min(which(sde<=0.4)) , min(which(spr<=0.4)))
	
	plot(fe, ye, type="l",xlab="", ylab="Equilibrium yield (million mt)", lwd=2)
	segments(fe[ix],0,fe[ix],ye[ix],lty=c(1, 2, 3))
	segments(0,ye[ix],fe[ix],ye[ix],lty=c(1, 2, 3))
	
	re <- re/re[1]
	plot(fe, re, type="l",xlab="", ylab="Relative recruitment", lwd=2) 
	segments(fe[ix],0,fe[ix],re[ix],lty=c(1, 2, 3)) 
	segments(0,re[ix],fe[ix],re[ix],lty=c(1, 2, 3)) 
	
	plot(fe, sde, type="l",xlab="", ylab="Spawning depletion", lwd=2)
	segments(fe[ix],0,fe[ix],sde[ix],lty=c(1, 2, 3))
	segments(0,sde[ix],fe[ix],sde[ix],lty=c(1, 2, 3))

	plot(fe, spr, type="l",xlab="", ylab="Spawning Potential Ratio", ylim=c(0, 1), lwd=2)
	segments(fe[ix],0,fe[ix],spr[ix],lty=c(1, 2, 3))
	segments(0,spr[ix],fe[ix],spr[ix],lty=c(1, 2, 3))   
	
	legend("topright", c("MSY", "SB40", "SPR40"), lty=1:3, bty="n")
	
	mtext("Equilibrium fishing mortality rate", 1, outer=T, line=-1)                               
	
  saveFig("Equilibrium")
	par(op)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~TABLES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

table.b <- function() {
	
	mcbt <- A$mc.sbt
	post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
	btci <- t(apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975)))
	
	filename <- paste(tabDir,"tableb_sbt.tex",sep="")
	filenamecsv <- paste(tabDir,"tableb_sbt.csv",sep="")
	t.b	<- cbind(A$yrs,btci[1:nyrs,])
	
	cap="Recent trends in estimated female spawning stock biomass (million mt) 
		based on 5000 systematic samples from the joint posterior distribution."
	cgrp <- c(" ","Female biomass")
	ncgrp	<- c(1,3)
	colnames(t.b)	<- c("Year",c("2.5%","median","97.5%")) 
		
  dum <- latex(tail(t.b, 10),file=filename,caption=cap,label="tableb",rowname=NULL,cgroup=cgrp,n.cgroup=ncgrp)
  write.csv(t.b,file=filenamecsv, row.names=F)
  cat(paste("Saved table ",filename,"...\n",sep=""))
  cat(paste("Saved table ",filenamecsv,"...\n",sep=""))
  
}	

table.c <- function() {
	
	mcdt <- A$mc.sbdepletion
	post.dt  <- as.data.frame(window(mcmc(mcdt),start=Burn,thin=Thin))
	dtci <- t(apply(post.dt,2,quantile,probs=c(0.025,0.5,0.975)))
	
	filename<-paste(tabDir,"tablec_depletion.tex",sep="")
	filenamecsv<-paste(tabDir,"tablec_depletion.csv",sep="")
	t.c	<- cbind(A$yrs,dtci[1:nyrs,])
	
	cap="Recent trends in estimated spawning depletion level
		based on 5000 systematic samples from the joint posterior distribution."
	cgrp <- c(" ","Depletion")
	ncgrp	<- c(1,3)
	colnames(t.c)	<- c("Year",c("2.5%","median","97.5%")) 
		
  dum <- latex(tail(t.c, 10),file=filename,caption=cap,label="tablec",rowname=NULL,cgroup=cgrp,n.cgroup=ncgrp)
  write.csv(t.c,file=filenamecsv, row.names=F)
#  cat(paste("Saved table ",filename,"...\n",sep=""))
#  cat(paste("Saved table ",filenamecsv,"...\n",sep=""))
}	

table.d <- function() {	
	mcrt <- A$mc.rt 
	post.rt  <- as.data.frame(window(mcmc(mcrt),start=Burn,thin=Thin))
	rtci <- t(apply(post.rt,2,quantile,probs=c(0.025,0.5,0.975)))
	
	filename<-paste(tabDir,"tabled_recruits.tex",sep="")
	filenamecsv<-paste(tabDir,"tabled_recruits.csv",sep="")
	t.d	<- cbind(ryr,rtci[1:length(ryr),])
	
	cap="Recent trends in estimated recruitment (billions of age 1 fish)
		based on 5000 systematic samples from the joint posterior distribution."
	cgrp <- c(" ","Recruits")
	ncgrp	<- c(1,3)
	colnames(t.d)	<- c("Year",c("2.5%","median","97.5%")) 
		
  dum <- latex(tail(t.d, 10),file=filename,caption=cap,label="tabled",rowname=NULL,cgroup=cgrp,n.cgroup=ncgrp)
  write.csv(t.d,file=filenamecsv, row.names=F)
  cat(paste("Saved table ",filename,"...\n",sep=""))
  cat(paste("Saved table ",filenamecsv,"...\n",sep=""))
}

table.e1 <- function() {
	spr <- A$mc.sprstatus_fmsy
	post.spr <- as.data.frame(window(mcmc(spr),start=Burn,thin=Thin))
	sprci <- t(apply(post.spr,2,quantile,probs=c(0.025,0.5,0.975)))
	
	filename<-paste(tabDir,"tablee_sprfmsy_status.tex",sep="")
	filenamecsv<-paste(tabDir,"tablee_sprfmsy_status.csv",sep="")
	t.e	<- cbind(A$yr,sprci)
	
	cap="Recent trends in (1-spr)/(1-spr at fmsy) based on 5000 systematic samples from the joint posterior distribution."
	cgrp <- c(" ","(1-spr)/(1-spr at fmsy)")
	ncgrp	<- c(1,3)
	colnames(t.e)	<- c("Year",c("2.5%","median","97.5%")) 
		
  dum <- latex(tail(t.e, 10),file=filename,caption=cap,label="tablee",rowname=NULL,cgroup=cgrp,n.cgroup=ncgrp)
  write.csv(t.e,file=filenamecsv, row.names=F)
  cat(paste("Saved table ",filename,"...\n",sep=""))
  cat(paste("Saved table ",filenamecsv,"...\n",sep=""))
}

table.e2 <- function() {	
	spr <- A$mc.sprstatus_f40
	post.spr <- as.data.frame(window(mcmc(spr),start=Burn,thin=Thin))
	sprci <- t(apply(post.spr,2,quantile,probs=c(0.025,0.5,0.975)))
	
	filename<-paste(tabDir,"tablee_sprf40_status.tex",sep="")
	filenamecsv<-paste(tabDir,"tablee_sprf40_status.csv",sep="")
	t.e	<- cbind(A$yr,sprci)
	
	cap="Recent trends in (1-spr)/(1-spr at f40) based on 5000 systematic samples from the joint posterior distribution."
	cgrp <- c(" ","(1-spr)/(1-spr at f40)")
	ncgrp	<- c(1,3)
	colnames(t.e)	<- c("Year",c("2.5%","median","97.5%")) 
		
  dum <- latex(tail(t.e, 10),file=filename,caption=cap,label="tablee",rowname=NULL,cgroup=cgrp,n.cgroup=ncgrp)
  write.csv(t.e,file=filenamecsv, row.names=F)
  cat(paste("Saved table ",filename,"...\n",sep=""))
  cat(paste("Saved table ",filenamecsv,"...\n",sep=""))
}	

table.f <- function() {
	
	bt3 <- A$mc.bt3
	ct <- A$ct[1,]
	cbt3 <- ct/bt3[, 1:length(A$yr)]
	post.cbt3 <- as.data.frame(window(mcmc(cbt3),start=Burn,thin=Thin))
	cbt3ci <- t(apply(post.cbt3,2,quantile,probs=c(0.025,0.5,0.975)))
	
	filename<-paste(tabDir,"tablef_exploitationFraction.tex",sep="")
	filenamecsv<-paste(tabDir,"tablef_exploitationFraction.csv",sep="")
	t.f	<- cbind(A$yr, cbt3ci)
	
	cap="Recent trends in Ct/Bt3+ based on 5000 systematic samples from the joint posterior distribution."
	cgrp <- c(" ","Ct/Bt3+")
	ncgrp	<- c(1,3)
	colnames(t.f)	<- c("Year",c("2.5%","median","97.5%")) 
		
  dum <- latex(tail(t.f, 10),file=filename,caption=cap,label="tablef",rowname=NULL,cgroup=cgrp,n.cgroup=ncgrp)
  write.csv(t.f,file=filenamecsv, row.names=F)
  cat(paste("Saved table ",filename,"...\n",sep=""))
  cat(paste("Saved table ",filenamecsv,"...\n",sep=""))
}	

table.h <- function(mle=T,tableType="ssb",perc=c(0.25,0.75),stock="Female",catchFactor=1e6,writeCSV=T){
  # tableDir:  absolute directory where the files will be stored
  # A:         an objects returned from reptolist, i.e. list of the rep file entries
  # Assumes A$mc.for exists and has the case sensetive column names: Year, CtStream, Rt, Sbt, f40spr, depletion, OY
  # streams: a vector of catch weights to include in the table. If a stream is less than zero, the OY will be used as the catch stream
  #          value in the table.  These f-based catch streams will be appended to the table in the order they were
  #          entered in the catchStreams vector.
  #          -They are found in the mcmc forecast file (tinss.for) in the CtStream column
  # mle: if True, use mle outputs.  If false, use MCMC output.
  # tableType = "ssb" means make table using spawning stock biomass (sbt column in A$mc.for) - makes es.table.h.1.csv
  # tableType = "depletion" means make table using reletive deletion (depletion column in A$mc.for) - makes es.table.h.2.csv
  # tableType = "f40spr" means make table using reletive spawning potential ratio (1-spr)/(1-0.4) (f40spr column in A$mc.for) - makes es.table.h.3.csv
  # stock is either "Female" or "All".  If Female the biomass is divided by 2.
  # catchFactor:  number to multiply catch by so it appears in the table in a nicer format
  # perc is a vector of the locations of the biomass vector to split the catch stream data on.

  #splits <- c(0,sort(perc),1)
  splits <- c(0,1)
  sections <- 1:(length(splits)-1)
  tmpMeds <- vector("numeric",length=length(sections))
  if(mle){
    forc <- A$mlefor
  }else{
    forc <- A$mcfor
  }
  years <- unique(forc$Year)
  streams <- unique(forc$CtStream)
  
  #medians <- as.data.frame(matrix(nrow=0,ncol=length(splits)+1))  # medians for the fixed catch streams
  #fmsyMedians <- as.data.frame(matrix(nrow=0,ncol=length(splits)+1)) # medians for the f-based catch streams
  #f40Medians <- as.data.frame(matrix(nrow=0,ncol=length(splits)+1)) # medians for the f-based catch streams
  #ssMedians <- as.data.frame(matrix(nrow=0,ncol=length(splits)+1)) # medians for the SS-OY-based catch streams
  
  medians <- as.data.frame(matrix(nrow=0,ncol=4))  # medians for the fixed catch streams
  fmsyMedians <- as.data.frame(matrix(nrow=0,ncol=4)) # medians for the f-based catch streams
  f40Medians <- as.data.frame(matrix(nrow=0,ncol=4)) # medians for the f-based catch streams
  ssMedians <- as.data.frame(matrix(nrow=0,ncol=4))# medians for the SS-OY-based catch streams

  for(year in years){
    for(stream in streams){
      yStream <- subset(forc,forc$Year==year & forc$CtStream==stream)
      specialStream <- median(as.numeric(yStream$OY))
      ctstream<-stream

      #colnames(medians) <- c("Year","CtStream",paste(splits[1:(length(splits)-1)]," - ",splits[2:length(splits)]))
      #colnames(fmsyMedians) <- c("Year","CtStream",paste(splits[1:(length(splits)-1)]," - ",splits[2:length(splits)]))
      #colnames(f40Medians) <- c("Year","CtStream",paste(splits[1:(length(splits)-1)]," - ",splits[2:length(splits)]))
      #colnames(ssMedians) <- c("Year","CtStream",paste(splits[1:(length(splits)-1)]," - ",splits[2:length(splits)]))
      
      colnames(medians) <- c("Year","CtStream","ABC","Median")
      colnames(fmsyMedians) <- c("Year","CtStream","ABC","Median")
      colnames(f40Medians) <- c("Year","CtStream","ABC","Median")
      colnames(ssMedians) <- c("Year","CtStream","ABC","Median")
            
      if(mle){
        sorted <- yStream
      }else{
        nsamp <- length(yStream[,1])
        yStream <- yStream[((Burn+1):nsamp),]
        sorted <- yStream #[order(yStream$Rt_2009),]
      }
        
      lenSorted <- nrow(sorted)
      for(section in sections){
        lims <- splits[section:(section+1)]
        lowerInd <- floor(lenSorted*as.numeric(lims[1]))
        upperInd <- floor(lenSorted*as.numeric(lims[2]))
        if(tableType=="ssb"){
          if(mle){
            tmpMeds[section] <- sorted$SBt            
          }else{
            tmpMeds[section] <- median(as.numeric(sorted[lowerInd:upperInd,]$Sbt))
          }
        }
        if(tableType=="depletion") {
          if(mle){
            tmpMeds[section] <- sorted$depletion
          }else{
            tmpMeds[section] <- median(as.numeric(sorted[lowerInd:upperInd,]$depletion))
          }
        }
        if(tableType== "f40spr") {
	          if(mle){
	            tmpMeds[section] <- sorted$SPR40status
	          }else{
	            tmpMeds[section] <- median(as.numeric(sorted[lowerInd:upperInd,]$SPR40status))
	          }
        }
        if(section==length(sections)){
          if(stream>=0){
            medians <- rbind(medians,as.numeric(c(year,ctstream,as.numeric(stream)*catchFactor,tmpMeds)))
          }
          if(stream==.FMSYFORCFLAG){
            fmsyMedians <- rbind(fmsyMedians,as.numeric(c(year,ctstream,specialStream*catchFactor,tmpMeds)))
          }
          if(stream==.F40FORCFLAG){
            f40Medians <- rbind(f40Medians,as.numeric(c(year,ctstream,specialStream*catchFactor,tmpMeds)))
          }
         if(stream==.SSFORCFLAG){
           ssMedians <- rbind(ssMedians,c(year,ctstream,specialStream*catchFactor,tmpMeds))
          }
        }
      }
    }
  }
  
  sort.medians.for.output <- medians[order(medians$CtStream,medians$Year),]
  sort.medians.for.output <- rbind(sort.medians.for.output,fmsyMedians)  # add fmsy-based catch streams
  sort.medians.for.output <- rbind(sort.medians.for.output,f40Medians)  # add f40-based catch streams
  sort.medians.for.output <- rbind(sort.medians.for.output,ssMedians)  # add SS-OY-based catch streams

  if(writeCSV){
    if(tableType=="ssb")
      fn <- paste(tabDir,"table.h1.ssb",sep="")
    if(tableType=="depletion")
      fn <- paste(tabDir,"table.h2.depletion.",sep="")
    if(tableType=="f40spr")
      fn <- paste(tabDir,"table.h3.f40spr",sep="")
    if(mle){
      fn <- paste(fn,"_mle.csv",sep="")
    }else{
      fn <- paste(fn,"_mcmc.csv",sep="")
    }
    # remove NAs from output
    sort.medians.for.output <- sort.medians.for.output[!is.na(sort.medians.for.output$CtStream),]
    sort.medians.for.output <- sort.medians.for.output[!is.na(sort.medians.for.output$Year),]
    write.csv(sort.medians.for.output,file=fn,row.names=F)
    cat(paste("Saved table ",fn,"...\n",sep=""))
  }
}

table.i <- function(){
	#This is the final summary table for the executive summary
	lbl <- c("Unfished SBo (million mt)", 
           "Unfished age-1 recruits (billions)", 
           "\\underline{\\emph{\\textbf{REFERENCE POINTS based on SB$_{40\\%}$}}}", 
           "MSY proxy spawning biomass SB$_{40\\%}$", 
           "SPR resulting in SB$_{40\\%}$", 
           "Exploitation fraction (ct/Bt3) resulting in SB$_{40\\%}$", 
           "Yield with SB$_{40\\%}$", 
           "\\underline{\\emph{\\textbf{REFERENCE POINTS based on SPR$_{40\\%}$}}}", 
           "Spawning biomass at SPR$_{40\\%}$", 
           "SPR", 
           "Exploitation fraction (ct/Bt3) resulting in SPR$_{40\\%}$", 
           "Yield with SPR$_{40\\%}$", 
           "\\underline{\\emph{\\textbf{REFERENCE POINTS based on MSY}}}", 
           "Spawning biomass at MSY", 
           "SPR at MSY", 
           "Exploitation fraction (ct/Bt3) at MSY", 
           "MSY")
  
  mcs <- A$mcRefPoints
  post.mcs <- as.data.frame(window(mcmc(mcs),start=Burn,thin=Thin))
  mcsci <- t(apply(post.mcs,2,quantile,probs=c(0.025,0.5,0.975)))
  ti <- mcsci
  ti <- rbind(ti[1:2, ], rep("", 3), ti[3:6, ], rep("", 3), ti[8:11, ], rep("", 3), ti[12:15, ])
  ti <- cbind(lbl, (ti))
  colnames(ti) <- c("Quantity", "2.5\\% percentile", "Median", "97.5\\% percentile")
  fn <- paste(tabDir,"table.i.csv",sep="")
  write.csv(ti,file=fn, row.names=F)
}

table.g1 <- function(){
  s <- read.csv(file="HakeManagement.csv")
  cap <- "Recent trend in Hake Management performance."
	filename <- paste(tabDir,"tablec.tex",sep="")
  dimnames(s)[[2]] <- c("Year","Landings","OY(mt)","ABC(mt)","Landings/OY(\\%)")
	latex(s,file=filename,caption=cap,label="tablec",rowname=NULL)  
}

table.g2<-function(){
	#This is a sideways table
  s <- read.csv(file="Summarytable.CSV")
	cap <- "Summary of recent trends in Pacific hake exploitation and stock levels."
	filename <- paste(tabDir,"tableg.tex",sep="")
  latex(s[, c(1, 2:11)],file=filename,caption=cap,label="tableg",rowname=NULL)
}

