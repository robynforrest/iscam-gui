saveFig <- function(filename){
  # Save the currently plotted figure to disk
  # in the 'figDir' folder with the name
  # filename.plotType
  if(saveon){
    filename <- paste(figDir,filename,".",plotType,sep="")
    savePlot(filename,type=plotType)
    cat(paste("Saved figure ",filename,"...\n",sep=""))
  }
}

fig.fishingMortality <- function(){
		op <- par(no.readonly=T)
		plot(A$yr,A$ft[1,],type="l",xlab="Year",ylab="Fishing Mortality (/yr) and Harvest Rate (%)", lwd=2, ylim=c(0,1.2*max(A$ft[1,])), xlim=c(A$yr[1],A$yr[length(A$yr)]), las=1)
		 Z<-A$ft[1,]+A$m
		 HR <- (A$ft[1,]/Z)*(1-exp(-Z))
		 lines(A$yr, HR, lwd=2, col="blue")
		 legend("topright", c("F", "HR"), col=c(1,"blue"), lwd=2, lty=1,bty="n")
		  
		  saveFig("fig.fishingMortalityMPD")
		  par(op)  
}

fig.RecAnomMPD <- function(){
	 op <- par(no.readonly=T)
	plot(A$yr,A$log_rec_devs,type="p",pch=20, main=paste("Predicted dt = ", A$dt), col=1, xlab="Year",ylab="MPD Recruitment anomalies",ylim=c(-5,5), xlim=c(A$yr[1],A$yr[length(A$yr)]), cex.lab=1.2, cex.axis=1.2,las=1)
	 points(A$yr,A$anom_obs,pch=4, col="darkblue")
	 points(A$yr,A$anom_obs*A$dt,pch=4, col=2)
	 abline(h=0, lty=2, lwd=0.5)
	 legend("topleft", legend=c("Predicted log rec deviations", "Additional log anomalies (obs)", "Additional log anomalies (obs) * dt (pred)"), lty=0, pch=c(20, 4,4), col=c(1,"darkblue",2), bty="n", cex=1.2)
	 saveFig("fig.RecAnomaliesMPD")
	  par(op)  
}

fig.RecAnomMCMC <- function(){
	 op <- par(no.readonly=T)
	mc <- log(A$mc.RecDevs)
	mc.recdev <- as.data.frame(window(mcmc(mc),start=Burn,thin=Thin)) 
	recdev <- apply(mc.recdev,2,quantile,probs=c(0.025,0.5,0.975)) #gets quantiles 
	
	#Get the observed anomalies scaled by estimates of dt
	#mc_ap <- matrix(ncol=length(A$anom_obs), nrow=length(A$mc$dt))
	#for(i in 1:length(A$mc$dt)) mc_ap[i,] <- A$anom_obs*A$mc$dt[i] #multiply the observed anomalies by the estimated mcmc values of the scalar dt
	
	#mc.anompred <- as.data.frame(window(mcmc(mc_ap),start=Burn,thin=Thin))
	#anompred <- apply(mc.anompred,2,quantile,probs=c(0.025,0.5,0.975)) #gets quantiles 
	
	
	plot(yr, recdev[2,], type="p", pch=20, lwd=2, ylim=c(-4,4),ylab="log recruitment deviations", xlab="Year",las=1)
	 arrows(yr, recdev[1, ],yr,recdev[3,],code=3,angle=90,length=0.01)
	 points(yr,A$anom_obs,pch=4, col="darkblue")
	 #points(yr,anompred[2, ],pch=4, col=2)
	#arrows(yr, anompred[1, ],yr,anompred[3,],code=3,angle=90,length=0.01, col=2)
	 abline(h=0, lty=2, lwd=0.5)
	# legend("topleft", legend=c("Predicted log rec deviations", "Additional log anomalies (obs)", "Additional log anomalies (obs) * dt (pred)"), lty=0, pch=c(20, 4,4), col=c(1,"darkblue",2), bty="n", cex=1.2)
	 saveFig("fig.RecAnomaliesMCMC")
	  par(op)  
}

fig.weightFit<- function(){
		 op <- par(no.readonly=T)
		 wyrs <- A$wyrs
		 wsig <- A$weight_sig
		 plot(wyrs,A$obs_annual_mean_wt,type="p",pch=20,xlab="Year",ylab="Annual mean weight",ylim=c(0,1.5*max(A$obs_annual_mean_wt)), xlim=c(A$yr[1],A$yr[length(A$yr)]),main="Mean weight", las=1)
		 arrows(wyrs, A$obs_annual_mean_wt + wsig*A$obs_annual_mean_wt,wyrs,A$obs_annual_mean_wt - wsig*A$obs_annual_mean_wt,code=3,angle=90,length=0.01)
		 lines(yr,A$annual_mean_wt[1:length(yr)], col="red", lwd=2)
		 legend("topleft", legend=c("Observed (+/- CV)","Predicted"), lty=c(0,1), lwd=c(0,2), pch=c(20, NA_integer_), col=c(1,"dark gray"), bty="n", cex=1.2)

		 saveFig("fig.annualMeanWt")
		  par(op)  
}

fig.catchFit <- function(){
 	  op <- par(no.readonly=T)
		plot(A$yr,A$ct[1,],type="l",col="dark gray", lwd=2, xlab="Year",ylab="Catch (t)",ylim=c(0,1.5*max(A$obs_ct,A$ct)), xlim=c(A$yr[1],A$yr[length(A$yr)]),main="Catch", las=1)
		points(A$yr,A$obs_ct[1,])
	  saveFig("fig.catchFit")
	  par(op)  
  }

fig.survey.age.residuals1	<-	function(){
	if(AgeLikelihood!=3){	
		op <- par(no.readonly=T)
		
		#counters
		ii <- 1
		ij<-A$na_nobs[1]
		
		for(k in 1:A$na_gears)
		{	
		
			Res <- Asurv_res[ii:ij,]
			iyr=Res[,1]
			if(length(iyr)<=4) par(mfcol=c(2,2), oma=c(2,3,1,1)) #4 graphs
			if(length(iyr)>4) par(mfcol=c(2,3), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2)) # 5 or 6
			if(length(iyr)>6) par(mfcol=c(3,3), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2)) #7,8 or 9
			if(length(iyr)>9) par(mfcol=c(3,4), oma=c(2,3,1,1), mai=c(0.25,0.2,0.2,0.2)) #10, 11 or 12
			if(length(iyr)>12) par(mfcol=c(4,4), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2)) #13-16
			if(length(iyr)>16) par(mfcol=c(5,5), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2)) #17-25

			  for(i in 1:length(iyr)){
				    r <- Res[i,3:(nage-age[1]+3)]
				    iclr <- r
				    iclr[r>=0] <- 1
				    iclr[iclr!=1] <- 2

				    plot(age,r,type="h",ylim=c(-6,6),main=iyr[i],col=iclr, las=1, xlab="", ylab="")
				    points(age,r,pch=19,col=iclr)
				}
				mtext("Residual",2,outer=T,las=3,line=1, cex=1.3)
				mtext("Age",1,outer=T,line=1, cex=1.3)

				 saveFig(paste("fig.survey.age.residuals1_survey_",k,sep=""))
				par(op)
				
				#update counters
				ii<-ij+1
				if(A$na_gears>1) {
					ij<-ij+A$na_nobs[k+1]
					if(k!=A$na_gears) 	windows()
				}	
		}	
	}else cat("WARNING: No plot for ageless model\n")  
}

fig.survey.age.residuals2 <-	function() {
	if(AgeLikelihood!=3){
		op <- par(no.readonly=T)
		#counters
		ii <- 1
		ij<-A$na_nobs[1]

		for(k in 1:A$na_gears)
		{	
			
			Res <- Asurv_res[ii:ij,]
			iyr=Res[,1]
			 r <- Res
			bubble.plot(r[,1],age,r[,3:(nage-age[1]+3)],scale=0.3,xlab="Year",ylab="Age",add=F,log.scale=T, las=1,cex.lab=1.3)
			par(mfcol=c(1,1))
			
		 	saveFig(paste("fig.survey.age.residuals2_survey_",k,sep=""))
				
			#update counters
			ii<-ij+1
			if(A$na_gears>1) {
				ij<-ij+A$na_nobs[k+1]
				if(k!=A$na_gears) 	windows()
			}	
		}	
		
		par(op)
  	}else cat("WARNING: No plot for ageless model\n")  
}

fig.survey.age.props	<-	function(){
	if(AgeLikelihood!=3){
		op <- par(no.readonly=T)
		#counters
		ii <- 1
		ij<-A$na_nobs[1]

		for(k in 1:A$na_gears)
		{	
			
			par(mfcol=c(1,1))
			Prop <- Asurv_obs[ii:ij,]
			iyr=Prop[,1]
			r <- Prop

			bubble.plot(r[,1],age,r[,3:(nage-age[1]+3)],scale=0.3,xlab="Year",ylab="Age",add=F,log.scale=T, las=1)
			
			saveFig(paste("fig.survey.age.props_survey_",k,sep=""))
			
			#update counters
			ii<-ij+1
			if(A$na_gears>1) {
				ij<-ij+A$na_nobs[k+1]
				if(k!=A$na_gears) 	windows()
			}
		}
	par(op)	
  	}else cat("WARNING: No plot for ageless model\n")  
}

fig.survey.age.props.fit	<-	function(){
	if(AgeLikelihood!=3){
			
		#Plots the collapsed conditional age-length keys to proportions at age in the survey
		op <- par(no.readonly=T)
		#counters
		ii <- 1
		ij<-A$na_nobs[1]

		for(k in 1:A$na_gears)
		{	
			
			Obs<- Asurv_obs[ii:ij,]
			Est<- Asurv_est[ii:ij,]
			Gear <- Asurv_obs[ii,2]
			iyr=Obs[,1]
			if(length(iyr)<=4) par(mfcol=c(2,2), oma=c(2,3,1,1)) #4 graphs
			if(length(iyr)>4) par(mfcol=c(2,3), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2)) # 5 or 6
			if(length(iyr)>6) par(mfcol=c(3,3), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2)) #7,8 or 9
			if(length(iyr)>9) par(mfcol=c(3,4), oma=c(2,3,1,1), mai=c(0.25,0.2,0.2,0.2)) #10, 11 or 12
			if(length(iyr)>12) par(mfcol=c(4,4), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2)) #13-16
			if(length(iyr)>16) par(mfcol=c(5,5), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2)) #17-25

		for(i in 1:length(iyr)){
				mp <- barplot(Obs[i,3:(nage-age[1]+3)],names.arg=paste(age),ylim=c(0.,0.8),main=iyr[i], las=1,legend.text="Observed", args.legend=c(bty="n"))
				lines(mp,Est[i,3:(nage-age[1]+3)],type="o",pch=20,cex=2,col=2)
				legend("topright", legend="Predicted",col=2, pch=19, bty="n")
		}
		mtext("Proportion",2,outer=T,las=3,line=2, cex=1.2)
		mtext("Age",1,outer=T,line=-1, cex=1.2)
		mtext(paste("Gear",Gear),3,outer=T, line=-1,cex=1.2)
		
		saveFig(paste("fig.survey.age.props.fit_survey_",k,sep=""))
		
		#update counters
		ii<-ij+1
		if(A$na_gears>1) {
			ij<-ij+A$na_nobs[k+1]
			if(k!=A$na_gears) 	windows()
		}
		
	}
		par(op)	
   }else cat("WARNING: No plot for ageless model\n")  
}

#RF NOTE - ADD AN MCMC VERSION OF THIS
fig.surveybiomass.fit	<-	function(){
	graphics.off()

	#get list of cvs from data file (horrible name format, will fix one day)
	Survdat<-as.matrix(A$dat$"#Survey")
	Survgears<-unique(Survdat[,3])
	#print(Survdat)

	op <- par(no.readonly=T)
	 if(A$nits==1){
		A$iyr<-t(as.matrix(A$iyr))
		A$it<-t(as.matrix(A$it))
		A$pit<-t(as.matrix(A$pit))}
	
	A$iyr1<-A$iyr[1,which(A$iyr[1,]>0)] #get rid of NAs
	A$pit1<-A$pit[1,which(A$pit[1,]>0)] #get rid of NAs
	A$it1<-A$it[1,which(A$it[1,]>0)] #get rid of NAs

	#Get annual CVs from survey1
	srows<-which(Survdat[,3]==Survgears[1])
	CV <- 1/(Survdat[srows,4] )
			
	plot(A$iyr1,A$pit1,type="l",col="dark gray", lwd=2,xlab="Year",ylab="Survey biomass index",main="Survey 1", ylim=c(0,max(A$it1+CV*A$it1)), xlim=c(A$iyr1[1],A$iyr1[length(A$iyr1)]), las=1)
	points(A$iyr1,A$it1, pch=19)
	arrows(A$iyr1,A$it1+CV*A$it1 ,A$iyr1,A$it1-CV*A$it1,code=3,angle=90,length=0.01)
  	saveFig("fig.surveybiomass.fit")
	
	if(A$nits>1){
		windows()
		A$iyr2<-A$iyr[2,which(A$iyr[2,]>0)] #get rid of NAs
		A$pit2<-A$pit[2,which(A$pit[2,]>0)] #get rid of NAs
		A$it2<-A$it[2,which(A$it[2,]>0)] #get rid of NAs

		#Get annual CVs from survey1
		srows<-which(Survdat[,3]==Survgears[2])
		CV <- 1/(Survdat[srows,4] )
				
		plot(A$iyr2,A$pit2,type="l",col="dark gray", lwd=2,xlab="Year",ylab="Survey biomass index",main="Survey 2", ylim=c(0,max(A$it2+CV*A$it2)), xlim=c(A$iyr2[1],A$iyr2[length(A$iyr2)]), las=1)
		points(A$iyr2,A$it2, pch=19)
		arrows(A$iyr2,A$it2+CV*A$it2 ,A$iyr2,A$it2-CV*A$it2,code=3,angle=90,length=0.01)
  		saveFig("fig.surveybiomass2.fit")
	}
	if(A$nits>2){
		windows()
		A$iyr3<-A$iyr[3,which(A$iyr[3,]>0)] #get rid of NAs
		A$pit3<-A$pit[3,which(A$pit[3,]>0)] #get rid of NAs
		A$it3<-A$it[3,which(A$it[3,]>0)] #get rid of NAs
		#Get annual CVs from survey1
		srows<-which(Survdat[,3]==Survgears[3])
		CV <- 1/(Survdat[srows,4] )
		plot(A$iyr3,A$pit3,col="dark gray", lwd=2,type="l",xlab="Year",ylab="Survey biomass index",main="Survey 3",ylim=c(0,max(A$it3+CV*A$it3)), xlim=c(A$iyr3[1],A$iyr3[length(A$iyr3)]), las=1)
		points(A$iyr3,A$it3, pch=19)
		arrows(A$iyr3,A$it3+CV*A$it3, A$iyr3,A$it3-CV*A$it3,code=3,angle=90,length=0.01)
  		saveFig("fig.surveybiomass3.fit")
	}
	if(A$nits==4){
		windows()
		A$iyr4<-A$iyr[4,which(A$iyr[4,]>0)] #get rid of NAs
		A$pit4<-A$pit[4,which(A$pit[4,]>0)] #get rid of NAs
		A$it4<-A$it[4,which(A$it[4,]>0)] #get rid of NAs
		#Get annual CVs from survey1
		srows<-which(Survdat[,3]==Survgears[4])
		CV <- 1/(Survdat[srows,4] )
		plot(A$iyr4,A$pit4,type="l",col="dark gray", lwd=2,xlab="Year",ylab="Survey biomass index",main="Survey 4",ylim=c(0,max(A$it4+CV*A$it4)), xlim=c(A$iyr4[1],A$iyr4[length(A$iyr4)]), las=1)
		points(A$iyr4,A$it4, pch=19)
		arrows(A$iyr4,A$it4+CV*A$it4 ,A$iyr4,A$it4-CV*A$it4,code=3,angle=90,length=0.01)
  		saveFig("fig.surveybiomass4.fit")
	}
	par(op)
}

fig.selectivity	<-	function(){
	op <- par(no.readonly=T)
	
	if(A$delaydiff ==0){
		sel<-A$selectivity
		vva<-sel[which(sel[,1]==1),] 
		va<-vva[length(vva[,1]),2:length(vva[1,])] #pick final year
		leg <- "Gear 1" #legend text
		cha <- 20 #plot character for legend
		
		plot(age, va,type="o",xlab="Age",ylab="Selectivity in final year", las=1, col=1, pch=cha) #ylim=c(0,1),
		for(i in 2:ngear) {	
			vva2<-sel[which(sel[,1]==i),] 
			va2<-vva2[length(vva2[,1]),2:length(vva2[1,])] #first column is gear id
			lines(age,va2,type="o",pch=i, lty=i, col=i)
			abline(h=0.5, lty=4, lwd=2, col="darkgray")
			leg <- c(leg,paste("Gear",i))
			cha <- c(cha,i)
		}
		leg <- c(leg,"50%")
		cha <- c(cha, -1)
		
		legend("bottomright",leg,lty=c(1:ngear,4),pch=cha,bty="n", cex=1.5, col=c(1:ngear,"darkgray"), lwd=c(rep(1,ngear),2))
		saveFig("fig.selectivities")
		par(op)
  	}else cat("WARNING: No estimated selectivity for delay difference model\n")
}

fig.phase <- function(yUpperLimit=2){
	#The phase plot of Bt/Bmsy vs (1-SPR)/(1-SPRmsy)
  # yUpperLimit is the upper limit of the y-axis
	op	<- par(no.readonly=T)
	spr <- A$sprmsy_status

	sbstatus <- A$sbtmsy_status
  sbstatus <- sbstatus[1:length(A$yr)]
  
  maxX <- max(sbstatus)
  maxY <- yUpperLimit

	plot(sbstatus, spr, type="n", col=1, ylim=c(0, maxY), xlim=c(0, maxX), xlab="B/BTarget", ylab="(1-SPR)/(1-SPRTarget)")
	#dum <- fried.egg(sbstatus, spr)

  lines(sbstatus, spr, type="o",col="blue")
  colVector <- vector(length=length(A$yr))
  colVector[1] <- "green"
  colVector[length(colVector)] <- "red"
  for(i in 2:(length(colVector)-1)){
    colVector[i] <- 0
  }
	points(sbstatus, spr, pch=20, col=colVector)
	abline(h=1, v=1, lty=3,col="red")

  saveFig("fig.phase")
	par(op)
  
}

fig.estimated.params.pairs <- function(ghatGear=1){
  # ghatGear: 1 is survey 2 is commercial
	#Pair plots of estimated parameters
	op <- par(no.readonly=T)

  nPar <- A$npar 
  if(A$nits==1) nPar <- nPar  # ( for q)
  if(A$nits==2) nPar <- nPar + 1 # (+1 for q)
  if(A$nits==3) nPar <- nPar + 2 # (+2 for q)
  if(A$nits==4) nPar <- nPar + 3 # (+3 for q)

#  if(ghatGear==1 & length(unique(A$mc$ghat.surv))>1){
#    nPar <- nPar + 1 # (+1 for ghat.surv or ghat.comm)
#  }
#  if(ghatGear==2 & length(unique(A$mc$ghat.comm))>1){
#    nPar <- nPar + 1 # (+1 for ghat.surv or ghat.comm)
#  }
  
  j <- matrix(ncol=nPar,nrow=length(Burn:nrow(A$mc)))
     j[,1] <- A$mc$log.ro[Burn:nrow(A$mc)] 
    j[,2] <- A$mc$h[Burn:nrow(A$mc)]
  j[,3] <- A$mc$log.m[Burn:nrow(A$mc)]
  j[,4] <- A$mc$log.rbar[Burn:nrow(A$mc)]
  j[,5] <- A$mc$log.rinit[Burn:nrow(A$mc)]
  j[,6] <- A$mc$rho[Burn:nrow(A$mc)]
  j[,7] <- A$mc$varphi[Burn:nrow(A$mc)]
  j[,8] <- A$mc$qc[Burn:nrow(A$mc)]
  j[,9] <- A$mc$q1[Burn:nrow(A$mc)]
  if(A$nits>1) j[,10] <- A$mc$q2[Burn:nrow(A$mc)]
  if(A$nits>2) j[,11] <- A$mc$q3[Burn:nrow(A$mc)]
  if(A$nits==4) j[,12] <- A$mc$q4[Burn:nrow(A$mc)]

 if(A$nits == 1) colnames(j) <- c("log(r0)","h","log(M)","log(rbar)","log(rinit)","rho","varphi","qc","q1")
  if(A$nits == 2) colnames(j)  <- c("log(r0)","h","log(M)","log(rbar)","log(rinit)","rho","varphi","qc","q1","q2")
  if(A$nits == 3) colnames(j)  <- c("log(r0)","h","log(M)","log(rbar)","log(rinit)","rho","varphi","qc","q1","q2","q3")
  if(A$nits == 4) colnames(j)  <- c("log(r0)","h","log(M)","log(rbar)","log(rinit)","rho","varphi","qc","q1","q2","q3","q4")

	pairs(j[,c(1:5,8:nPar)], pch=".",upper.panel=panel.smooth,diag.panel=panel.hist, lower.panel=panel.smooth)
  saveFig("fig.estimated.params.pairs")
	par(op)
}

#no logs
fig.estimated.params.pairs2 <- function(ghatGear=1){
  # ghatGear: 1 is survey 2 is commercial
	#Pair plots of estimated parameters
	op <- par(no.readonly=T)

  nPar <- A$npar
  if(A$nits==1) nPar <- nPar  # ( for q)
  if(A$nits==2) nPar <- nPar + 1 # (+1 for q)
  if(A$nits==3) nPar <- nPar + 2 # (+2 for q)
  if(A$nits==4) nPar <- nPar + 3 # (+3 for q))

#if(ghatGear==1 & length(unique(A$mc$ghat.surv))>1){
#	nPar <- nPar + 1 # (+1 for ghat.surv or ghat.comm)
#	}
#if(ghatGear==2 & length(unique(A$mc$ghat.comm))>1){
#	nPar <- nPar + 1 # (+1 for ghat.surv or ghat.comm)
#	}
  j <- matrix(ncol=nPar,nrow=length(Burn:nrow(A$mc)))
    j[,1] <-  exp(A$mc$log.ro[Burn:nrow(A$mc)]) #oops! this was plotting msy and fmsy instead of ro and h! fixed!
    j[,2] <- A$mc$h[Burn:nrow(A$mc)]
  j[,3] <- exp(A$mc$log.m[Burn:nrow(A$mc)])
  j[,4] <- exp(A$mc$log.rbar[Burn:nrow(A$mc)])
  j[,5] <- exp(A$mc$log.rinit[Burn:nrow(A$mc)])
  j[,6] <- A$mc$rho[Burn:nrow(A$mc)]
   j[,7] <- A$mc$varphi[Burn:nrow(A$mc)]
   j[,8] <- A$mc$qc[Burn:nrow(A$mc)]
  j[,9] <- A$mc$q1[Burn:nrow(A$mc)]
  if(A$nits>1) j[,10] <- A$mc$q2[Burn:nrow(A$mc)]
    if(A$nits>2) j[,11] <- A$mc$q3[Burn:nrow(A$mc)]
  if(A$nits==4) j[,12] <- A$mc$q4[Burn:nrow(A$mc)]
 
  if(A$nits == 1) colnames(j) <- c("r0","h","M","rbar","rinit","rho","varphi","qc","q1")
   if(A$nits == 2) colnames(j)  <- c("r0","h","M","rbar","rinit","rho","varphi","qc","q1","q2")
   if(A$nits == 3) colnames(j)  <- c("r0","h","M","rbar","rinit","rho","varphi","qc","q1","q2","q3")
  if(A$nits == 4) colnames(j)  <- c("r0","h","M","rbar","rinit","rho","varphi","qc","q1","q2","q3","q4")
  
#    if(length(unique(A$mc$ghat.surv))>1 & ghatGear==1){ # survey gear
#      nPar <- nPar + 1
#      j[,nPar + 1] <- A$mc$ghat.surv[Burn:nrow(A$mc)]
#      colnames <- c(colnames,"ghat.surv")
#    }
#    if(length(unique(A$mc$ghat.comm))>1 & ghatGear==2){ # commercial gear
#      nPar <- nPar + 1    
#      j[,nPar + 1] <- A$mc$ghat.comm[Burn:nrow(A$mc)]
#      colnames <- c(colnames,"ghat.comm")
#  }
	
	pairs(j[,c(1:5,8:nPar)], pch=".",upper.panel=panel.smooth,diag.panel=panel.hist, lower.panel=panel.smooth) #lower.panel=bi.level
        saveFig("fig.estimated.params.pairs.no.log")
	par(op)
  
}

#key parameters only 
fig.estimated.params.pairs.key <- function(ghatGear=1){
  	#Pair plots of estimated parameters
	op <- par(no.readonly=T)

  nPar <- A$npar
  if(A$nits==1) nPar <- nPar-3  # ( for q)	  (not plotting dt, rho, kappa or qc)
  if(A$nits==2) nPar <- nPar-2  # (+1 for q)
  if(A$nits==3) nPar <- nPar - 1 # (+2 for q)
  if(A$nits==4) nPar <- nPar  # (+3 for q))

  j <- matrix(ncol=nPar,nrow=length(Burn:nrow(A$mc)))
    j[,1] <-  A$mc$log.ro[Burn:nrow(A$mc)]
    j[,2] <- A$mc$h[Burn:nrow(A$mc)]
  j[,3] <- A$mc$log.m[Burn:nrow(A$mc)]
  j[,4] <- A$mc$log.rbar[Burn:nrow(A$mc)]
  j[,5] <- A$mc$log.rinit[Burn:nrow(A$mc)]
  j[,6] <- A$mc$q1[Burn:nrow(A$mc)]
  if(A$nits>1) j[,7] <- A$mc$q2[Burn:nrow(A$mc)]
    if(A$nits>2) j[,8] <- A$mc$q3[Burn:nrow(A$mc)]
  if(A$nits==4) j[,9] <- A$mc$q4[Burn:nrow(A$mc)]
 
 if(A$nits == 1) colnames(j) <- c("log(r0)","h","log(M)","log(rbar)","log(rinit)","q1")
  if(A$nits == 2) colnames(j)  <- c("log(r0)","h","log(M)","log(rbar)","log(rinit)","q1","q2")
  if(A$nits == 3) colnames(j)  <- c("log(r0)","h","log(M)","log(rbar)","log(rinit)","q1","q2","q3")
  if(A$nits == 4) colnames(j)  <- c("log(r0)","h","log(M)","log(rbar)","log(rinit)","q1","q2","q3","q4")
 
	pairs(j, pch=".",upper.panel=panel.smooth,diag.panel=panel.hist, lower.panel=panel.smooth) #lower.panel=bi.level
        saveFig("fig.key.estimated.params.pairs")
	par(op)
  
}

#key parameters only no log
fig.estimated.params.pairs.no.log.key <- function(ghatGear=1){
  # ghatGear: 1 is survey 2 is commercial
	#Pair plots of estimated parameters
	op <- par(no.readonly=T)

  nPar <- A$npar
  if(A$nits==1) nPar <- nPar-3  # ( for q)	  (not plotting dt, rho, kappa or qc)
  if(A$nits==2) nPar <- nPar-2  # (+1 for q)
  if(A$nits==3) nPar <- nPar - 1 # (+2 for q)
  if(A$nits==4) nPar <- nPar  # (+3 for q))

  j <- matrix(ncol=nPar,nrow=length(Burn:nrow(A$mc)))
    j[,1] <-  exp(A$mc$log.ro[Burn:nrow(A$mc)]) #oops! this was plotting msy and fmsy instead of ro and h! fixed!
    j[,2] <- A$mc$h[Burn:nrow(A$mc)]
  j[,3] <- exp(A$mc$log.m[Burn:nrow(A$mc)])
  j[,4] <- exp(A$mc$log.rbar[Burn:nrow(A$mc)])
  j[,5] <- exp(A$mc$log.rinit[Burn:nrow(A$mc)])
  j[,6] <- A$mc$q1[Burn:nrow(A$mc)]
  if(A$nits>1) j[,7] <- A$mc$q2[Burn:nrow(A$mc)]
    if(A$nits>2) j[,8] <- A$mc$q3[Burn:nrow(A$mc)]
  if(A$nits==4) j[,9] <- A$mc$q4[Burn:nrow(A$mc)]
 
  if(A$nits == 1) colnames(j) <- c("r0","h","M","rbar","rinit","q1")
   if(A$nits == 2) colnames(j)  <- c("r0","h","M","rbar","rinit","q1","q2")
   if(A$nits == 3) colnames(j)  <- c("r0","h","M","rbar","rinit","q1","q2","q3")
  if(A$nits == 4) colnames(j)  <- c("r0","h","M","rbar","rinit","q1","q2","q3","q4")
  
	
	pairs(j, pch=".",upper.panel=panel.smooth,diag.panel=panel.hist, lower.panel=panel.smooth) #lower.panel=bi.level
        saveFig("fig.key.estimated.params.pairs.no.log")
	par(op)
  
}

fig.mcmc.priors.vs.posts <- function(exFactor=1.0, qPriorFunction=4,ghatGear=1,ghatPriorFunction=4,showEntirePrior=T){
  # exFactor is a multiplier for the minimum and maximum xlims.
  # qPriorFunction: 4 for gamma
  # ghat=1 for survey selectivity, 2 for commercial selectivity
  # showEntirePrior, if T then plot the entire prior function to its limits
  #  and ignore posterior distribution limits
	op	<- par(no.readonly=T)
  #  plus one in code to compensate for the transfer from 0-based to 1-based
  qPriorFunction <- qPriorFunction + 1
 
  # npar is the number of parameters with priors + 1 for q which is seperate from the others
  nPar <- A$npar 
   if(A$nits==1) nPar <- nPar  # ( for q)
   if(A$nits==2) nPar <- nPar + 1 # (+1 for q)
   if(A$nits==3) nPar <- nPar + 2 # (+2 for q)
  if(A$nits==4) nPar <- nPar + 3 # (+3 for q)
 
  d <- matrix(ncol=nPar,nrow=length(Burn:nrow(A$mc)))
  mle <- vector(mode="numeric",length=nPar)
  # The values in the REPORT file for each of priorN are:
  # 1. ival  = initial value
  # 2. lb    = lower bound
  # 3. ub    = upper bound
  # 4. phz   = phase
  # 5. prior = prior distribution funnction
  #             0 = Uniform
  #             1 = normal    (p1=mu,p2=sig)
  #             2 = lognormal (p1=log(mu),p2=sig)
  #             3 = beta      (p1=alpha,p2=beta)
  #             4 = gamma     (p1=alpha,p2=beta)
  # 6. p1 (defined by 5 above)
  # 7. p2 (defined by 5 above)
  functionNames <- c(dunif,dnorm,dlnorm,dbeta,dgamma)
  functionNamesR <- c(runif,rnorm,rlnorm,rbeta,rgamma)
  # Set up column names depending on switches
    d[,1] <- A$mc$log.ro[Burn:nrow(A$mc)]
    d[,2] <- A$mc$h[Burn:nrow(A$mc)]
  d[,3] <- A$mc$log.m[Burn:nrow(A$mc)]
  d[,4] <- A$mc$log.rbar[Burn:nrow(A$mc)]
  d[,5] <- A$mc$log.rinit[Burn:nrow(A$mc)]
  d[,6] <- A$mc$rho[Burn:nrow(A$mc)]
  d[,7] <- A$mc$varphi[Burn:nrow(A$mc)]
  d[,8] <- A$mc$qc[Burn:nrow(A$mc)]
   d[,9] <- log(A$mc$q1[Burn:nrow(A$mc)])
  if(A$nits>1) d[,10] <- log(A$mc$q2[Burn:nrow(A$mc)])
  if(A$nits>2) d[,11] <- log(A$mc$q3[Burn:nrow(A$mc)])
  if(A$nits==4) d[,12] <- log(A$mc$q4[Burn:nrow(A$mc)])
  if(A$nits == 1) colnames(d) <- c("ln(r0)","h","ln(M)","ln(rbar)","ln(rinit)","rho","varphi","qc","ln(q1)")
  if(A$nits == 2) colnames(d) <- c("ln(r0)","h","ln(M)","ln(rbar)","ln(rinit)","rho","varphi","qc","ln(q1)","ln(q2)")
  if(A$nits == 3) colnames(d) <- c("ln(r0)","h","ln(M)","ln(rbar)","ln(rinit)","rho","varphi","qc","ln(q1)","ln(q2)","ln(q3)")
  if(A$nits == 4) colnames(d) <- c("ln(r0)","h","ln(M)","ln(rbar)","ln(rinit)","rho","varphi","qc","ln(q1)","ln(q2)","ln(q3)","ln(q4)")

   # Set up prior function names 
  mle[1] <- A$theta[1]
  mle[2] <- A$theta[2]
  mle[3] <- A$theta[3]
  mle[4] <- A$theta[4]
  mle[5] <- A$theta[5]
  mle[6] <- A$theta[6]
  mle[7] <- A$theta[7]
   mle[8] <- A$theta[8]
  if(A$nits == 1) mle[9] <- log(A$q[1])
  if(A$nits > 1) mle[10] <- log(A$q[2])
  if(A$nits > 2) mle[11] <- log(A$q[3])
  if(A$nits == 4) mle[12] <- log(A$q[4])
 
 
  post.samp	<- window(mcmc(d),start=Burn,thin=Thin)
  colnames(post.samp) <- colnames(d)
  nm <- colnames(d)
  # the following are +1 because they are 0-based in the CTL file,
  # and R's vectors are 1-based
  fn <- c(functionNames[A$priorPars_theta1[5]+1],
          functionNames[A$priorPars_theta2[5]+1],
          functionNames[A$priorPars_theta3[5]+1],
          functionNames[A$priorPars_theta4[5]+1],
          functionNames[A$priorPars_theta5[5]+1],
          functionNames[A$priorPars_theta6[5]+1],
          functionNames[A$priorPars_theta7[5]+1],
          functionNames[A$priorPars_theta8[5]+1],
          functionNames[A$q_prior[1:A$nits]+1])
  mu <- c(A$priorPars_theta1[6],
          A$priorPars_theta2[6],
          A$priorPars_theta3[6],
          A$priorPars_theta4[6],
          A$priorPars_theta5[6],
          A$priorPars_theta6[6],
          A$priorPars_theta7[6],
          A$priorPars_theta8[6],
           A$q_mu)
  sig <- c(A$priorPars_theta1[7],
           A$priorPars_theta2[7],
           A$priorPars_theta3[7],
           A$priorPars_theta4[7],
           A$priorPars_theta5[7],
           A$priorPars_theta6[7],
           A$priorPars_theta7[7],
           A$priorPars_theta8[7],
           A$q_sd)
  fnr <- c(functionNamesR[A$priorPars_theta1[5]+1],
          functionNamesR[A$priorPars_theta2[5]+1],
          functionNamesR[A$priorPars_theta3[5]+1],
          functionNamesR[A$priorPars_theta4[5]+1],
          functionNamesR[A$priorPars_theta5[5]+1],
          functionNamesR[A$priorPars_theta6[5]+1],
          functionNamesR[A$priorPars_theta7[5]+1],
          functionNamesR[A$priorPars_theta8[5]+1],
          functionNamesR[A$q_prior[1:A$nits]+1])         

  plotPar = c(1:5,9:nPar)
  if(length(plotPar<=9)) par(mfrow=c(3,3),mai=c(0.3,0.2,0.3,0.2), omi=c(0.2,0.2,0.2,0.2))  # show all parameters both fixed and estimated
  else	 par(mfrow=c(4,4),mai=c(0.3,0.2,0.3,0.2), omi=c(0.2,0.2,0.2,0.2))
  
  for(param in plotPar){
		x <- list(p=d[,param],mu=mu[param],sig=sig[param],fn=fn[[param]],nm=nm[param],mle=mle[param])
    		plot.marg(x,breaks="sturges",col="wheat",exFactor=exFactor)
  }
  saveFig("fig.mcmc.priors.vs.posts")
  par(op)
   windows() 

 #Plot priors on their own
  if(length(plotPar<=9)) par(mfrow=c(3,3),mai=c(0.3,0.2,0.3,0.2), omi=c(0.2,0.2,0.2,0.2))  # show all parameters both fixed and estimated
	else	 par(mfrow=c(4,4),mai=c(0.3,0.2,0.3,0.2), omi=c(0.2,0.2,0.2,0.2))

	 n<-0
	 exfactor=c(1,1.1,2.75,1,1,10,10,1)
	 for(param in plotPar){
		   n=n+1
		   px <- list(p=d[,param],mu=mu[param],sig=sig[param],fn=fn[[param]],nm=nm[param],mle=mle[param],fnr=fnr[[param]])
		     plot.prior(px,breaks="sturges",col="green",exFactor=exfactor[n]) 
	 }
	 saveFig("fig.priors")
	par(op)
  
}


plot.marg <- function(xx,breaks="sturges",exFactor=1.0,...){
  #xx is a list(p=samples, mu=prior mean, s=prior varian, fn=prior distribution)
  # exFactor is a multiplier for the minimum and maximum xlims.
  # showEntirePrior, if T then plot the entire prior function to its limits
  #  and ignore posterior distribution limits
  ssNoPlot <- hist(xx$p,breaks=breaks,plot=F)
  xl <- seq(min(ssNoPlot$breaks)/exFactor,max(ssNoPlot$breaks)*exFactor,length=250)
 
 pd <- xx$fn(xl,xx$mu,xx$sig)
   z <- cbind(xl,pd)
  Xlim <- c(min(xl),max(xl))
  ss <- hist(xx$p,prob=T,breaks=breaks,main=xx$nm,xlab="", cex.axis=1.2,xlim=Xlim,...)#,
  lines(xl,pd,col="green",lwd=2)     
  abline(v=xx$mle, lwd=2, lty=2, col=2)
 
}

plot.prior <- function(xx,breaks="sturges",exFactor=1.0,...){
	  #xx is a list(p=samples, mu=prior mean, s=prior varian, fn=prior distribution)
	  randsamp<-xx$fnr(10000,  xx$mu, xx$sig) #get random sample
	   xl <- seq(min(randsamp),max(randsamp),length=250)
          pd <- xx$fn(xl,xx$mu,xx$sig)
	 plot(xl,pd,col=1,lwd=2, type="l", ,main=xx$nm, las=1, cex.axis=1.2)     
  }



 #NO LOGS OR PRIORS 
fig.mcmc.priors.vs.posts2 <- function(exFactor=1.0,qPriorFunction=4,ghatGear=1,ghatPriorFunction=4){

 # exFactor is a multiplier for the minimum and maximum xlims.
 
	op	<- par(no.readonly=T)
 
  par(mfrow=c(4,4),mai=c(0.3,0.2,0.3,0.2), omi=c(0.2,0.3,0.2,0.2))  # show all parameters both fixed and estimated 
  # npar is the number of parameters with priors + 1 for q which is seperate from the others
  nPar <- A$npar +4 # plotting 4 ref points
   if(A$nits==1) nPar <- nPar  # ( for q)
   if(A$nits==2) nPar <- nPar + 1 # (+1 for q)
   if(A$nits==3) nPar <- nPar + 2 # (+2 for q)
  if(A$nits==4) nPar <- nPar + 3 # (+3 for q)
 
  d <- matrix(ncol=nPar,nrow=length(Burn:nrow(A$mc)))
  mle <- vector(mode="numeric",length=nPar)

  # Set up column names depending on switches
  d[,1] <- exp(A$mc$log.ro[Burn:nrow(A$mc)])
  d[,2] <- A$mc$h[Burn:nrow(A$mc)]
  d[,3] <-  exp(A$mc$log.m[Burn:nrow(A$mc)])
  d[,4] <-  exp(A$mc$log.rbar[Burn:nrow(A$mc)])
  d[,5] <-  exp(A$mc$log.rinit[Burn:nrow(A$mc)])
  d[,6] <- A$mc$rho[Burn:nrow(A$mc)]
  d[,7] <- A$mc$varphi[Burn:nrow(A$mc)]
  d[,8] <- A$mc$qc[Burn:nrow(A$mc)]
  d[,9] <- A$mc$q1[Burn:nrow(A$mc)]
  if(A$nits==1){
	   d[,10] <- A$mc$bo[Burn:nrow(A$mc)]
	   d[,11] <- A$mc$bmsy[Burn:nrow(A$mc)]
	   d[,12] <- A$mc$msy[Burn:nrow(A$mc)]
	    d[,13] <- A$mc$fmsy[Burn:nrow(A$mc)]
	    colnames(d) <- c("r0","h","M","rbar","rinit","rho","varphi","qc","q1", "bo", "bmsy", "msy", "fmsy")
  } 
  if(A$nits==2){
    	   d[,10] <- A$mc$q2[Burn:nrow(A$mc)]
    	   d[,11] <- A$mc$bo[Burn:nrow(A$mc)]
  	   d[,12] <- A$mc$bmsy[Burn:nrow(A$mc)]
  	   d[,13] <- A$mc$msy[Burn:nrow(A$mc)]
  	   d[,14] <- A$mc$fmsy[Burn:nrow(A$mc)]
  	   colnames(d)  <- c("r0","h","M","rbar","rinit","rho","varphi","qc","q1","q2", "bo", "bmsy", "msy", "fmsy")
  } 
  if(A$nits==3){
    	   d[,10] <- A$mc$q2[Burn:nrow(A$mc)]
    	   d[,11] <- A$mc$q3[Burn:nrow(A$mc)]
    	   d[,12] <- A$mc$bo[Burn:nrow(A$mc)]
  	   d[,13] <- A$mc$bmsy[Burn:nrow(A$mc)]
  	   d[,14] <- A$mc$msy[Burn:nrow(A$mc)]
  	   d[,15] <- A$mc$fmsy[Burn:nrow(A$mc)]
  	   colnames(d)  <- c("r0","h","M","rbar","rinit","rho","varphi","qc","q1","q2","q3", "bo", "bmsy", "msy", "fmsy")
  } 
  if(A$nits==4){
    	   d[,10] <- A$mc$q2[Burn:nrow(A$mc)]
    	   d[,11] <- A$mc$q3[Burn:nrow(A$mc)]
    	   d[,12] <- A$mc$q4[Burn:nrow(A$mc)]
    	   d[,13] <- A$mc$bo[Burn:nrow(A$mc)]
  	   d[,14] <- A$mc$bmsy[Burn:nrow(A$mc)]
  	   d[,15] <- A$mc$msy[Burn:nrow(A$mc)]
  	   d[,16] <- A$mc$fmsy[Burn:nrow(A$mc)]
  	   colnames(d)  <- c("r0","h","M","rbar","rinit","rho","varphi","qc","q1","q2","q3","q4", "bo", "bmsy", "msy", "fmsy")
  }
  
    # Set up mpd
    mle[1] <- exp(A$theta[1])
    mle[2] <- A$theta[2]
    mle[3] <- exp(A$theta[3])
    mle[4] <- exp(A$theta[4])
    mle[5] <- exp(A$theta[5])
    mle[6] <- A$theta[6]
    mle[7] <- A$theta[7]
     mle[8] <- A$theta[8]
    if(A$nits == 1) mle[9] <- A$q[1]
    if(A$nits > 1) mle[14] <- A$q[2]
    if(A$nits > 2) mle[15] <- A$q[3]
    if(A$nits == 4) mle[16] <- A$q[4]
  
    post.samp	<- window(mcmc(d),start=Burn,thin=Thin)
	colnames(post.samp) <- colnames(d)
 	nm <- colnames(d)
  
    for(param in 1:nPar){
		x <- list(p=d[,param],nm=nm[param],mle=mle[param])
    		plot.marg2(x,breaks=17,col="wheat",exFactor=exFactor,showEntirePrior)
  }
  saveFig("fig.mcmc.priors.vs.posts2")
	par(op)
  
}

 #NO LOGS OR PRIORS 
fig.mcmc.priors.vs.postskey <- function(exFactor=1.0,qPriorFunction=4,ghatGear=1,ghatPriorFunction=4){

 # exFactor is a multiplier for the minimum and maximum xlims.
 
	op	<- par(no.readonly=T)
 
  par(mfrow=c(4,4),mai=c(0.3,0.2,0.3,0.2), omi=c(0.2,0.3,0.2,0.2))  # show all parameters both fixed and estimated 
  # npar is the number of parameters with priors + 1 for q which is seperate from the others
   nPar <- A$npar
   if(A$nits==1) nPar <- nPar-3  # ( for q)	 not plotting rho, kappa or qc
   if(A$nits==2) nPar <- nPar -2 # (+1 for q)
   if(A$nits==3) nPar <- nPar -1 # (+2 for q)
  if(A$nits==4) nPar <- nPar  # (+3 for q)
    nPar <- nPar +4 # plotting 4 ref points
 
  d <- matrix(ncol=nPar,nrow=length(Burn:nrow(A$mc)))
  mle <- vector(mode="numeric",length=nPar)

  # Set up column names depending on switches
  d[,1] <- exp(A$mc$log.ro[Burn:nrow(A$mc)])
  d[,2] <- A$mc$h[Burn:nrow(A$mc)]
  d[,3] <-  exp(A$mc$log.m[Burn:nrow(A$mc)])
  d[,4] <-  exp(A$mc$log.rbar[Burn:nrow(A$mc)])
  d[,5] <-  exp(A$mc$log.rinit[Burn:nrow(A$mc)])
  d[,6] <- A$mc$q1[Burn:nrow(A$mc)]
  if(A$nits==1){
	   d[,7] <- A$mc$bo[Burn:nrow(A$mc)]
	   d[,8] <- A$mc$bmsy[Burn:nrow(A$mc)]
	   d[,9] <- A$mc$msy[Burn:nrow(A$mc)]
	    d[,10] <- A$mc$fmsy[Burn:nrow(A$mc)]
	    colnames(d) <- c("r0","h","M","rbar","rinit","q1", "bo", "bmsy", "msy", "fmsy")
  } 
  if(A$nits==2){
    	   d[,7] <- A$mc$q2[Burn:nrow(A$mc)]
    	   d[,8] <- A$mc$bo[Burn:nrow(A$mc)]
  	   d[,9] <- A$mc$bmsy[Burn:nrow(A$mc)]
  	   d[,10] <- A$mc$msy[Burn:nrow(A$mc)]
  	   d[,11] <- A$mc$fmsy[Burn:nrow(A$mc)]
  	   colnames(d)  <- c("r0","h","M","rbar","rinit","q1","q2", "bo", "bmsy", "msy", "fmsy")
  } 
  if(A$nits==3){
    	   d[,7] <- A$mc$q2[Burn:nrow(A$mc)]
    	   d[,8] <- A$mc$q3[Burn:nrow(A$mc)]
    	   d[,9] <- A$mc$bo[Burn:nrow(A$mc)]
  	   d[,10] <- A$mc$bmsy[Burn:nrow(A$mc)]
  	   d[,11] <- A$mc$msy[Burn:nrow(A$mc)]
  	   d[,12] <- A$mc$fmsy[Burn:nrow(A$mc)]
  	   colnames(d)  <- c("r0","h","M","rbar","rinit","q1","q2","q3", "bo", "bmsy", "msy", "fmsy")
  } 
  if(A$nits==4){
    	   d[,7] <- A$mc$q2[Burn:nrow(A$mc)]
    	   d[,8] <- A$mc$q3[Burn:nrow(A$mc)]
    	   d[,9] <- A$mc$q4[Burn:nrow(A$mc)]
    	   d[,10] <- A$mc$bo[Burn:nrow(A$mc)]
  	   d[,11] <- A$mc$bmsy[Burn:nrow(A$mc)]
  	   d[,12] <- A$mc$msy[Burn:nrow(A$mc)]
  	   d[,13] <- A$mc$fmsy[Burn:nrow(A$mc)]
  	   colnames(d)  <- c("r0","h","M","rbar","rinit","q1","q2","q3","q4", "bo", "bmsy", "msy", "fmsy")
  }
  
    # Set up mpd
    mle[1] <- exp(A$theta[1])
    mle[2] <- A$theta[2]
    mle[3] <- exp(A$theta[3])
    mle[4] <- exp(A$theta[4])
    mle[5] <- exp(A$theta[5])
    mle[6] <- A$theta[6]
    if(A$nits == 1) mle[7] <- A$q[1]
    if(A$nits > 1) mle[8] <- A$q[2]
    if(A$nits > 2) mle[9] <- A$q[3]
    if(A$nits == 4) mle[10] <- A$q[4]
  
    post.samp	<- window(mcmc(d),start=Burn,thin=Thin)
	colnames(post.samp) <- colnames(d)
 	nm <- colnames(d)
  
    for(param in 1:nPar){
		x <- list(p=d[,param],nm=nm[param],mle=mle[param])
    		plot.marg2(x,breaks=17,col="wheat",exFactor=exFactor,showEntirePrior)
  }
  saveFig("fig.mcmc.priors.vs.postskey")
	par(op)
  
}


plot.marg2 <- function(xx,breaks="sturges",exFactor=1.0,showEntirePrior=F,...){
  #xx is a list(p=samples, mu=prior mean, s=prior varian, fn=prior distribution)
  # exFactor is a multiplier for the minimum and maximum xlims.
  # showEntirePrior, if T then plot the entire prior function to its limits
  #  and ignore posterior distribution limits
	ssNoPlot <- hist(xx$p,breaks=breaks,plot=F)
  xl <- seq(min(ssNoPlot$breaks),max(ssNoPlot$breaks),length=250)
 
  ss <- hist(xx$p,prob=T,breaks=breaks,main=xx$nm,xlab="",...)
  abline(v=xx$mle, lwd=2, lty=2, col=2)
}



fig.mcmc.trace <- function(){
	op	<- par(no.readonly=T) 
	par(mfrow=c(3, 3), las=1)
  npar<-length(A$mc[1,])
  plotpar<-1:5
  if(nits==1)   plotpar <- c(plotpar,(npar-1))
   if(nits==2)  plotpar <- c(plotpar,(npar-2),(npar-1))
    if(nits==3) plotpar <- c(plotpar,(npar-3),(npar-2),(npar-1))
     if(nits==4) plotpar <- c(plotpar,(npar-4),(npar-3),(npar-2),(npar-1))
 
 mcmcData <- window(mcmc(A$mc[,plotpar]), start=Burn, thin=Thin)
  for(param in 1:length(plotpar)){
    par(mar=c(2,3,2,2))
    mcmcTrace <- as.matrix(mcmcData[,param])
    plot(mcmcTrace,main=colnames(mcmcData)[param],type="l",ylab="",xlab="",axes=F)
    box()
    at <- seq(0,end(mcmcData)-start(mcmcData),200)
    labels <- seq(start(mcmcData),end(mcmcData),200)
    axis(1,at=at,labels=labels)
    axis(2)
  }
  saveFig("fig.mcmc.trace")
	par(op)
  
}

fig.mcmc.density <- function(color=1,opacity="20"){
	op	<- par(no.readonly=T) 
	par(mfrow=c(4, 4), las=1)
  mcmcData <- window(mcmc(A$mc[,1:13]), start=Burn, thin=Thin)
  for(param in 1:ncol(mcmcData)){
    par(mar=c(2,3,2,2))
    dens <- density(mcmcData[,param])
    plot(dens,main=colnames(mcmcData)[param])
    xx <- c(dens$x,rev(dens$x))
    yy <- c(rep(min(dens$y),length(dens$y)),rev(dens$y))
    shade <- getShade(color,opacity)
    polygon(xx,yy,density=NA,col=shade)
  }
  saveFig("fig.mcmc.density")
	par(op)
}

fig.mcmc.autocor <- function(){
 npar<-length(A$mc[1,])
  plotpar<-1:5
  if(nits==1)   plotpar <- c(plotpar,(npar-1))
   if(nits==2)  plotpar <- c(plotpar,(npar-2),(npar-1))
    if(nits==3) plotpar <- c(plotpar,(npar-3),(npar-2),(npar-1))
     if(nits==4) plotpar <- c(plotpar,(npar-4),(npar-3),(npar-2),(npar-1))

  par(mfrow=c(3, 3), las=1,mar=c(3,3,2,2))
  Lags=  c(0, 1, 5, 10, 15,20,30,40,50)

  lags <- matrix(nrow=length(Lags), ncol=length(plotpar))
  colnames(lags)=colnames(A$mc[,plotpar])

   n <- 0
  for(i in plotpar){
	    n <- n+1
	    x <- window(mcmc(A$mc[,i]),start=Burn,thin=Thin)
	    ac <- autocorr(x, lags = Lags, relative=TRUE)
	    lags[,n] <- ac

	    autocorr.plot(x, lag.max=100, main=paste(colnames(lags)[n]), auto.layout=F)
  }
  saveFig("fig.mcmc.autocor")
  #par(op)
}

fig.mcmc.gelman <- function(){
  x <- window(mcmc(A$mc[,1:12]),start=Burn+1,thin=Thin)
  gelman.plot(x,bin.width = 10,max.bins = 50,confidence = 0.95, transform = FALSE, autoburnin=TRUE, auto.layout = TRUE)
  saveFig("fig.mcmc.gelman")
}

fig.mcmc.geweke <- function(frac1=0.1,frac2=0.5,nbins=20,pvalue=0.05,silent=F, useShades=F, ...){
	op	<- par(no.readonly=T)
  on.exit(par(op))
  
  x <- window(mcmc(A$mc[,1:12]),start=Burn+1,thin=Thin)

  #x <- mcmc(read.table("gewekeExample.csv",sep=",",header=T)[,1:10])

  par(mfrow=c(4,3), las=1)

  ystart <- seq(from = start(x), to = (start(x) + end(x))/2, length = nbins)
  gcd <- array(dim = c(length(ystart), nvar(x), nchain(x)), 
               dimnames = c(ystart, varnames(x), chanames(x)))

  for (i in 1:length(ystart)) {
    geweke.out <- try(geweke.diag(window(x, start = ystart[i]),frac1 = frac1, frac2 = frac2), silent=silent)
    for (k in 1:nchain(x)){
      gcd[i, , k] <- geweke.out[[k]]
    }
  }

  climit <- qnorm(1 - pvalue/2)
  for (k in 1:nchain(x)){
    for (j in 1:nvar(x)) {
      par(mar=c(2,3,2,2))
      ylimit <- max(c(climit, abs(gcd[, j, k])))
      plot(ystart, gcd[, j, k], type = "p", xlab = "Iteration", 
           ylab = "Z-score", pch = 1, ylim = c(-ylimit, ylimit), col="blue", 
           ...)
      lines(ystart,gcd[,,1][,j],lwd=2, col="blue")
      if(useShades){
        xx <- c(ystart,rev(ystart))
        yy <- c(rep(100*max(gcd[,,1][,j]),length(gcd[,,1][,j])),rev(gcd[,,1][,j]))
        shade <- getShade("green","15")
        polygon(xx,yy,density=NA,col=shade)

        xx <- c(ystart,rev(ystart))
        yy <- c(rep(100*min(gcd[,,1][,j]),length(gcd[,,1][,j])),rev(gcd[,,1][,j]))
        shade <- getShade("blue","15")
        polygon(xx,yy,density=NA,col=shade)

      }
      abline(h = c(climit, -climit), lty = 2)
      if (nchain(x) > 1) {
        title(main = paste(varnames(x, allow.null = FALSE)[j], 
                " (", chanames(x, allow.null = FALSE)[k], ")", 
                sep = ""))
      }
      else {
        title(main = paste(varnames(x, allow.null = FALSE)[j], 
                sep = ""))
      }
    }
  }
  saveFig("fig.mcmc.geweke")
	par(op)
}

fig.variance.partitions <- function(){
  op <- par(no.readonly=T)
	rho <- A$mc$rho[Burn:nrow(A$mc)]
	varphi <- 1/A$mc$varphi[Burn:nrow(A$mc)]
	sig <- rho*varphi
	tau <- (1-rho)*varphi
	d <- cbind(vp=varphi,sig,tau)
	pairs(d, pch=".", upper.panel=NULL, gap=0)
  saveFig("fig.variance.partitions")
	par(op)
  
}

fig.time.varying.selectivity	<- function(gear=1){
	if(A$delaydiff==0){
			with(A, {
				plot.sel<-function(x, y, z, ...){
					z <- z/max(z)
					z0 <- 0 #min(z) - 20
					z <- rbind(z0, cbind(z0, z, z0), z0)
					x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
					y <- c(min(y) - 1e-10, y, max(y) + 1e-10)
					clr=colorRampPalette(c("honeydew","lawngreen"))
					nbcol=50
					iclr=clr(nbcol)
					nrz <- nrow(z)
					ncz <- ncol(z)
					zfacet <- z[-1, -1]+z[-1, -ncz]+z[-nrz, -1]+z[-nrz, -ncz]
					facetcol <- cut(zfacet, nbcol)
					fill <- matrix(iclr[facetcol],nr=nrow(z)-1,nc=ncol(z)-1)
					fill[ , i2 <- c(1,ncol(fill))] <- "white"
					fill[i1 <- c(1,nrow(fill)) , ] <- "white"

					par(bg = "transparent")
					persp(x, y, z, theta = 35, phi = 25, col = fill, expand=5, 
						shade=0.75,ltheta=45 , scale = FALSE, axes = TRUE, d=1,  
						xlab="Year",ylab="Age",zlab="Selectivity", 
						ticktype="detailed", ...)
				}
				ix <- 1:length(yr)
		    plot.sel(yr, age, exp(log_sel[log_sel[,1]==gear,-1]),main=paste("Gear", gear))

			})
		  if(gear==1){ # WARNING - assumes gears laid out a certain way !!!
		    saveFig("fig.time.varying.comm.sel")
		  }else if(gear==2){
		    saveFig("fig.time.varying.surv.sel")
		  } 
	  }else cat("WARNING: No estimated selectivity for delay difference model\n")
  
}

plotRuntimeStats <- function(type=0,ylab=""){
  # plots runtime stats for all scenarios.
  # assumes all scenarios have the same number of projected years
  # assumes the opList structure is used.
  # types:
  # 1 = objFun, 2 = max gradient, 3 = number of function evals, 4 = hangcode, any other value = exit code
  
  if(type==1 | type==2 | type==3){ # use PLOTBUBBLES from PBSModelling for these ones
    if(type==1){
      # Objective function values
      # GREEN means value is positive GOOD
      # RED means a bad objective function value, i.e. returned -1,#IND (which is represented as 0.0 from GrMPE)
      dat <- abs(opList[[1]][[4]]$ObjectiveFunction)
      for(scenario in 2:length(opList)){
        dat <- rbind(dat,abs(opList[[scenario]][[4]]$ObjectiveFunction))
      }
      dat <- t(dat)
      colnames(dat) <- 1:length(opList)
      rownames(dat) <- ""
      plotBubbles(dat,dnam=F,cpro=F,ylab=ylab,clrs=c("green","red","black"),xaxt='n',yaxt='n')
      text(1:length(opList),1.04,dat,srt=-45,adj=1)
      text(1:length(opList),1,1:length(opList))
      title("Objective function values")
    }else if(type==2){
      # Maximum Gradient values
      # GREEN represents a good gradient, i.e. one that is smaller than .maxGrad
      # RED represents anything greater than .MAXGRAD
      dat <- abs(opList[[1]][[4]]$MaxGrad)
      for(scenario in 2:length(opList)){
        dat <- rbind(dat,opList[[scenario]][[4]]$MaxGrad)
      }
      dat <- t(dat)
      colnames(dat) <- 1:length(opList)
      rownames(dat) <- ""
      dat <- ifelse(dat>.MAXGRAD,0,dat)
      dat <- ifelse(dat<.MAXGRAD,dat,-dat)
      plotBubbles(dat,dnam=F,cpro=F,ylab=ylab,clrs=c("green","red","red"),xaxt='n',yaxt='n') 
      text(1:length(opList),1.04,dat,srt=-45,adj=1)
      text(1:length(opList),1,1:length(opList))
      title(paste("Maximum gradient values (<",.MAXGRAD,")"))
    }else if(type==3){
      # Number of function evaluations
      # GREEN means the number of function evaluations was greater than .FUNEVALS
      # RED means the number of function evaluations was less than .FUNEVALS
      dat <- opList[[1]][[4]]$nf
      for(rep in 2:length(opList)){
        dat <- rbind(dat,opList[[rep]][[4]]$nf)
      }
      dat <- t(dat)
      colnames(dat) <- 1:length(opList)
      rownames(dat) <- ""
      dat <- ifelse(dat<.FUNEVALS,-dat,dat)
      plotBubbles(dat,dnam=F,cpro=F,ylab=ylab,clrs=c("green","red","black"),xaxt='n',yaxt='n')
      text(1:length(opList),1.04,dat,srt=-45,adj=1)
      text(1:length(opList),1,1:length(opList))
      title(paste("Number of function evaluations (>",.FUNEVALS,")"))
    }
  }else{
  # NOT USING PLOTBUBBLES!!
    if(type==4){
      # Hang codes
      plotcharCol <- ifelse(opList[[1]][[4]]$HangCode==1,"red","green")
      # GREEN means no error condition
      # RED means no improvement in function value when 10th to last value compared with
      #     current value.
    }else{
      # Exit codes
      plotcharCol <- ifelse(opList[[1]][[4]]$ExitCode==1,"green","red")
      # GREEN for normal exit - i.e. all derivatives satisfy conditions
      # RED for problem with the initial estimate for the Hessian matrix.
      #     - The hessian matrix must be positive definite
      plotcharCol <- ifelse(opList[[1]][[4]]$ExitCode==2,"blue",plotcharCol)
      # BLUE for problem with the derivatives, either:
      # a) There is an error in the derivatives or
      # b) function does not decrease in direction of search, perhaps due to numerical
      #    round off error, or too stringent a convergence criterion
      plotcharCol <- ifelse(opList[[1]][[4]]$ExitCode==3,"purple",plotcharCol)
      # PURPLE for Maximum number of function calls exceeded
    }
#  par( oma=c(2,2,4,1), mar=c(3,3,3,1), mfrow=c(1,1) )
    plot(1,1,
       pch=.PCHCODE,
       xlab="Scenario",
       ylab=ylab,
       col=plotcharCol,
       xlim=c(1,length(opList)),
       ylim=c(1,1))
    for(rep in 2:length(opList)){
      if(type==4){
        plotcharCol <- ifelse(opList[[rep]][[4]]$HangCode==1,"red","green")
        title("Hang code values")
      }else{
        plotcharCol <- ifelse(opList[[rep]][[4]]$ExitCode==1,"green","red")
        plotcharCol <- ifelse(opList[[rep]][[4]]$ExitCode==2,"blue",plotcharCol)
        plotcharCol <- ifelse(opList[[rep]][[4]]$ExitCode==3,"purple",plotcharCol)
        title("Exit code values")
      }
      points(rep,1,pch=.PCHCODE,col=plotcharCol)
    }
  }
  
}

 #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 #New figures for Pacific cod performance measures
fig.Allcontrol.pts.Box <- function(){
 	graphics.off()
 	#This function takes the output from ADMB projections and plots the distribution of alternative control points
 	#Control points are benchmark variables that do not change with tac
 	#The mcmc data files has nrow =ntac x nctlpts	 only take the first tac level (tac=0) as these control points are insensitive to future tac (all based on historical catch/biomass/F)
 	ddmcmc <- A$mcproj 
 	ddmpd <- A$mpdproj
 	ctlpts<-c(2,5,8,12,14,16,18,20,22) #columns of projection file that contain control points
 	Bctl <- ctlpts[c(1,3,5,6,8)]
	Fctl <- ctlpts[c(2,4,7,9)]
 	dmcmc <- subset(ddmcmc, tac==0)	   #take only the first tac from each posterior sample - the control points do not change with tac
 	nsamp <-length(ddmcmc[,1])
 	
    	mcmcDataB <- dmcmc[(Burn+1):nsamp,Bctl]
	mcmcDataF <- dmcmc[(Burn+1):nsamp,Fctl]
	
	#Calculate and add 0.8BMSY and 0.4BMSY to data
	Bmsy08 <- 0.8*mcmcDataB[,2]
	Bmsy04 <- 0.4*mcmcDataB[,2]
	mcmcDataB<-cbind(mcmcDataB[,1:2],Bmsy08, Bmsy04,mcmcDataB[,3:5])
	#colnames(mcmcDataB) <- c(colnames(mcmcDataB), "0.8BMSY", "0.4BMSY")
	
 	for(i in 1:2){
 		if(i==1) {
 			boxplot(mcmcDataB/1000, names=colnames(mcmcDataB), range=0.95,pch=".", col="darkgray", las=1, xlab="Control points", ylab= "Biomass (x 1000 t)", main="Biomass-based control points", ylim=c(0,75)) #
  			saveFig(paste("fig.ctlpt.Box",i, sep=""))
  			windows()
		}else 
		{	boxplot(mcmcDataF, names=colnames(mcmcDataF), pch=".", range=0.95, col="darkgray", las=1, xlab="Control points", ylab= "Fishing mortality (/yr)", main="F-based control points")
         		saveFig(paste("fig.ctlpt.Box",i, sep=""))}
	  }
 	#par(op)
 }

 fig.MSYcontrol.pts <- function(){
	graphics.off()
	#This function takes the output from ADMB projections and plots the distribution of alternative control points
	#Control points are benchmark variables that do not change with tac
	#The mcmc data files has nrow =ntac x nctlpts	 only take the first tac level (tac=0) as these control points are insensitive to future tac (all based on historical catch/biomass/F)
	ddmcmc <- A$mcproj 
	ddmpd <- A$mpdproj
	ctlpts<-c(8,12) #columns of projection file that contain control points
	dmcmc <- subset(ddmcmc, tac==0)	   #take only the first tac from each posterior sample - the control points do not change with tac
	 colour=0
	  	
   	for(i in ctlpts) {
		colour=colour+1
		mcmcData <- window(mcmc(dmcmc[,i]), start=Burn, thin=Thin)
		dens<-density(mcmcData)
		Med <- quantile(mcmcData, na.rm=T,0.5)	  	 #Use quantiles to get the median. The median calculated from density is a bit off
		#Median <- median(mcmcData, na.rm=T)	 #test
		mpdCtlPts <- ddmpd[1,i] #take only the first tac for the MPD
		
		plot(dens,main=colnames(ddmcmc)[i], xlab=colnames(ddmcmc)[i], ylab="Density",las=1)
		 xx <- c(dens$x,rev(dens$x))
		 yy <- c(rep(min(dens$y),length(dens$y)),rev(dens$y))
		 shade <- getShade(colour,20)
     		polygon(xx,yy,density=NA,col=shade)
		abline(v=mpdCtlPts, col=2,lwd=2,lty=2)
		if(colour < length(ctlpts))	windows()
  	}
	for(i in length(ctlpts)) saveFig(paste("fig.ctlpt.MSY.density",i, sep=""))
	#par(op)
}
  
fig.Histcontrol.pts <- function(){
	     graphics.off()
	#This function takes the output from ADMB projections and plots the distribution of alternative control points
	#Control points are benchmark variables that do not change with tac
	#The mcmc data files has nrow =ntac x nctlpts	 only take the first tac level (tac=0) as these control points are insensitive to future tac (all based on historical catch/biomass/F)
	ddmcmc <- A$mcproj 
	ddmpd <- A$mpdproj
	ctlpts<-c(14,16,18,20,22) #columns of projection file that contain control points
	dmcmc <- subset(ddmcmc, tac==0)	   #take only the first tac from each posterior sample - the control points do not change with tac
	 colour=0
	
	for(i in ctlpts) {
		colour=colour+1
		mcmcData <- window(mcmc(dmcmc[,i]), start=Burn, thin=Thin)
		dens<-density(mcmcData)
		Med <- quantile(mcmcData, na.rm=T,0.5)	  	 #Use quantiles to get the median. The median calculated from density is a bit off
		#Median <- median(mcmcData, na.rm=T)	 #test
		mpdCtlPts <- ddmpd[1,i] #take only the first tac for the MPD

		plot(dens,main=colnames(ddmcmc)[i], xlab=colnames(ddmcmc)[i], ylab="Density",las=1)
		 xx <- c(dens$x,rev(dens$x))
		 yy <- c(rep(min(dens$y),length(dens$y)),rev(dens$y))
		 shade <- getShade(1,20)
		polygon(xx,yy,density=NA,col=shade)
		abline(v=mpdCtlPts, col=2,lwd=2,lty=2)
		if(colour < length(ctlpts))	windows()
	  }
	for(i in length(ctlpts)) saveFig(paste("fig.ctlpt.Hist.density",i, sep=""))
	#par(op)
 }

fig.Benchmarks <- function(){
 		graphics.off()
 		#This function takes the output from ADMB projections and plots the distribution of alternative control points
		#Control points are benchmark variables that do not change with tac
		#The mcmc data files has nrow =ntac x nctlpts	 only take the first tac level (tac=0) as these control points are insensitive to future tac (all based on historical catch/biomass/F)
		ddmcmc <- A$mcproj 
		ddmpd <- A$mpdproj
		ctlpts<-c(2,5) #columns of projection file that contain control points
		dmcmc <- subset(ddmcmc, tac==0)	   #take only the first tac from each posterior sample - the control points do not change with tac
		 colour=0
			   	
	   	for(i in ctlpts) {
			colour=colour+1
			mcmcData <- window(mcmc(dmcmc[,i]), start=Burn, thin=Thin)
			dens<-density(mcmcData)
			Med <- quantile(mcmcData, na.rm=T,0.5)	  	 #Use quantiles to get the median. The median calculated from density is a bit off
			#Median <- median(mcmcData, na.rm=T)	 #test
			mpdCtlPts <- ddmpd[1,i] #take only the first tac for the MPD
			
			plot(dens,main=colnames(ddmcmc)[i], xlab=colnames(ddmcmc)[i], ylab="Density",las=1)
			 xx <- c(dens$x,rev(dens$x))
			 yy <- c(rep(min(dens$y),length(dens$y)),rev(dens$y))
			 shade <- getShade(colour,20)
	     		polygon(xx,yy,density=NA,col=shade)
			abline(v=mpdCtlPts, col=2,lwd=2,lty=2)
			if(colour < length(ctlpts))	windows()
	  	}
		for(i in length(ctlpts)) saveFig(paste("fig.ctlpt.Benchmarks.density",i, sep=""))
	#par(op)
 }


######~~~~~~~~Unused in 2012 so far~~~~~~~~~~~###################################

fig.mcmc.diagnostics	<-	function(){
  #This function runs diagnostic plots for the posterior samples
  op <- par(no.readonly=T)
  if(length(unique(A$mc$abar))==1){
    # abar was not estimated, so don't include it
    if(length(unique(A$mc$gbar))==1){
      # gbar was not estimated, so don't include it
      np <- c(1:5,9)
    }else{
      np <- c(1:6,9)
    }
  }else{
    np <- c(1:7,9)
  }

	xyplot(post.samp[,np])
	acfplot(post.samp[,1:np])
  saveFig("fig.mcmc.diagnostics")
	par(op)
  
}

fig.fmsy.steepness <- function(){
  op <- par(no.readanly=T)
	par(mfrow=c(2,2), mai=c(0.75,0.751,0.15,0.15), omi=c(0.35,0.45,0.15,0.15), las=1)
	fmc=A$mc[,2]
	hmc=A$mc[,11]
	CRmc=4*hmc/(1-hmc)
	Mmc=A$mc[,3]

	hist(fmc,prob=T,breaks=30,xlab="Fmsy",ylab="Posterior density Fmsy",main="",col="wheat", cex.axis=1.1, cex.lab=1.1)
	lines(density(fmc,na.rm=T),col="red")
	mtext(paste("Mean =",round(mean(fmc),2), " Median =", round(median(fmc),2), "SD =", round(sd(fmc),2)), side=3, line=-0.5, cex=0.8)

	hist(hmc,prob=T,breaks=30,xlab="h",ylab="Posterior density Steepness",main="", xlim=c(0.2,1),col="wheat", cex.axis=1.1, cex.lab=1.1)
	lines(density(hmc,na.rm=T),col="red")
	mtext(paste("Mean =",round(mean(hmc),2), " Median =", round(median(hmc),2), "SD =", round(sd(hmc),2)), side=3, line=-0.5, cex=0.8)

	hist(CRmc,prob=T,breaks=30,xlab="CR",ylab="Posterior density Compensation Ratio",main="",col="wheat", cex.axis=1.1, cex.lab=1.1)
	lines(density(CRmc,na.rm=T),col="red")
	mtext(paste("Mean =",round(mean(CRmc),2), " Median =", round(median(CRmc),2), "SD =", round(sd(CRmc),2)), side=3, line=-0.5, cex=0.8)
	
	hist(Mmc,prob=T,breaks=30,xlab="M",ylab="Posterior density M",main="",col="wheat", cex.axis=1.1, cex.lab=1.1)
	lines(density(Mmc,na.rm=T),col="red")
	mtext(paste("Mean =",round(mean(Mmc),2), " Median =", round(median(Mmc),2), "SD =", round(sd(Mmc),2)), side=3, line=-0.5, cex=0.8)
  saveFig("fig.fmsy.steepness")

  par(op)
  
}

fig.spr.vs.management.target <- function(){
	#The relative spawning potential ratio (1-spr)/(1-spr.at.msy)  
	op	<- par(no.readonly=T)
	spr <- A$mc.f40spr #read.table("tinss.f40spr",h=F)
	post.spr <- as.data.frame(window(mcmc(spr),start=Burn,thin=thin))
	sprci <- apply(post.spr,2,quantile,probs=c(0.025,0.5,0.975))
	matplot(A$yr,t(sprci),type="l",col=c(2,1,2),lty=c(3,1,3), lwd=2, pch=c(-1, 0, 1),ylim=c(0,max(sprci))
		,xlab="Year",ylab="SPR")
	abline(h=1)
	text(1980, 1, "Management target", pos=3)
	par(op)
  
}

fig.yields.4panel <- function(A,type){
	#plot the equilibrium yield curves  
	op <- par(no.readonly=T)
	par(mfcol=c(2,2))
	#A$equil comes from the TINSS.rep file
	fe <- A$equil[, 1]
	ye <- A$equil[, 2]
	de <- A$equil[, 3]
	spr <- A$equil[, 4]
	
	plot(fe, ye, type="l", xlab="Fishing mortality (Fe)", ylab="Equilibrium yield"); gletter(1)
	plot(de, ye, type="l", xlab="Spawning depletion", ylab="Equilibrium yield", lty=2, col=2);gletter(2)
	plot(spr,ye, type="l", xlab="Spawning potential ratio", ylab="Equilibrium yield", lty=3, col=3);gletter(3)
	matplot(cbind(fe, de, spr), ye/max(ye)*100, type="l",xlab="Fe, depletion,  SPR",  ylab="Relative equilibrium yield")
	gletter(4)
  saveFig("fig.yields.4panel")
	par(op)
  
}

fig.yield.depletion.relrecuitment.spr <- function(){
	#Relationship between fishing mortlaity ~ yield,  recruitment,  SBe,  SPR
	op <- par(no.readonly=T)
	par(mfcol=c(2,2))
	fe <- A$equil[, 1]
	ye <- A$equil[, 2]
	de <- A$equil[, 3]
	spr <- A$equil[, 4]
	re <- A$equil[, 5]  
	ix <- c(min(which(ye==max(ye))),min(which(de<=0.4)) , min(which(spr<=0.4)))
	plot(fe, ye, type="l",xlab="", ylab="Equilibrium yield (million mt)", lwd=2)
	segments(fe[ix],0,fe[ix],ye[ix],lty=c(1, 2, 3))
	segments(0,ye[ix],fe[ix],ye[ix],lty=c(1, 2, 3))
	
	re <- re/re[1]
	plot(fe, re, type="l",xlab="", ylab="Relative recruitment", lwd=2) 
	segments(fe[ix],0,fe[ix],re[ix],lty=c(1, 2, 3)) 
	segments(0,re[ix],fe[ix],re[ix],lty=c(1, 2, 3)) 
	
	plot(fe, de, type="l",xlab="", ylab="Spawning depletion", lwd=2)
	segments(fe[ix],0,fe[ix],de[ix],lty=c(1, 2, 3))
	segments(0,de[ix],fe[ix],de[ix],lty=c(1, 2, 3))

	plot(fe, spr, type="l",xlab="", ylab="Spawning Potential Ratio", ylim=c(0, 1), lwd=2)
	segments(fe[ix],0,fe[ix],spr[ix],lty=c(1, 2, 3))
	segments(0,spr[ix],fe[ix],spr[ix],lty=c(1, 2, 3))   
	
	legend("topright", c("MSY", "SB40", "SPR40"), lty=1:3, bty="n")
	
	mtext("Equilibrium fishing mortality rate", 1, outer=T, line=-1)                               
  saveFig("fig.yield.depletion.relrecruitment.spr")
	par(op)
}

gweke.chain <- function (x, frac1 = 0.1, frac2 = 0.5, nbins = Nbin, pvalue = 0.05, auto.layout = TRUE, ...){
    x <- as.mcmc.list(x)
    oldpar <- NULL
    on.exit(par(oldpar))
    if (auto.layout) 
        oldpar <- par(mfrow = set.mfrow(Nchains = nchain(x), 
            Nparms = nvar(x)))
    ystart <- seq(from = start(x), to = (start(x) + end(x))/2, 
        length = nbins)
    if (is.R()) 
        gcd <- array(dim = c(length(ystart), nvar(x), nchain(x)), 
            dimnames = c(ystart, varnames(x), chanames(x)))
    else gcd <- array(dim = c(length(ystart), nvar(x), nchain(x)), 
        dimnames = list(ystart, varnames(x), chanames(x)))
    for (n in 1:length(ystart)) {
        geweke.out <- geweke.diag(window(x, start = ystart[n]), 
            frac1 = frac1, frac2 = frac2)
        for (k in 1:nchain(x)) gcd[n, , k] <- geweke.out[[k]]$z
    }
    climit <- qnorm(1 - pvalue/2)
    for (k in 1:nchain(x)) for (j in 1:nvar(x)) {
        ylimit <- max(c(climit, abs(gcd[, j, k])))
        plot(ystart, gcd[, j, k], type = "p", xlab = "First iteration in segment", 
            ylab = "Z-score", pch = 4, ylim = c(-ylimit, ylimit), 
            ...)
        abline(h = c(climit, -climit), lty = 2)
        if (nchain(x) > 1) {
            title(main = paste(varnames(x, allow.null = FALSE)[j], 
                " (", chanames(x, allow.null = FALSE)[k], ")", 
                sep = ""))
        }
        else {
            title(main = paste(varnames(x, allow.null = FALSE)[j], 
                sep = ""))
        }
        if (k == 1 && j == 1) 
            oldpar <- c(oldpar, par(ask = ask))
    }
 }

