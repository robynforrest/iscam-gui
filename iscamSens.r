#**********************************************************************************
# ccamSens.r
# This file contains the code necessary to plot sensitivities overlaid on top
# of one another by sensitivity group which is set in the file 'SensitivityGroup'
# in each Scenario folder.  Scenarios with the same sensitivity group will be plotted
# together.  The 'SensitivityGroup' file should have a single number in it, nothing more.
# The Base model will be SensitivityGroup=0 and will be plotted against all other
# Sensitivity groups.
# 
# Assumes the opList has been built and contains the SensitivityGroup element:
# opList[[scenario]][[4]]$SensitivityGroup
#
# Author            : Chris Grandin
# Development Date  : December 2011 - January 2012
#
#**********************************************************************************

fig.base.vs.sens <- function(sensitivityGroup=1,whichPlot="biomass",ylimit=6,useMaxYlim=T,lty=2,lwd=2,pch=20,offset=0.3,opacity="20"){
  # plots Spawning stock biomiass, depletion, and recruitment for a given sensitivity group
  #  - plot the MCMC posterior data for each, with confidence limits
  #  - offset is the number of years to offset each vertical bar from each other.
  #  - opacity is a two digit string from 00-99 
  # whichPlot can be:
  # 1. "biomass"
  # 2. "depletion"
  # 3. "recruits"
  
  op	<- par(no.readonly=T)
  base <- 0
  color <- 1
  colors <- color

  for(scenario in 1:length(opList)){
    if(opList[[scenario]][[4]]$SensitivityGroup == 0){
      base <- scenario
    }
  }
  plotR <- T
  baseRep <- opList[[base]][[4]]
  runNames <- strsplit(opList[[base]][[1]],"/")[[1]][3] # Gets the run's folder name out of the reletive path
  ##RECRUITS  
  if(whichPlot == "recruits"){
  	
  	#Get the maximum year from the longest time series for xlim - i.e. might be comparing results from different lengths of data
  	#Need to do this here so the xlim is long enough
    	for(scenario in 1:length(opList)){
	      if(scenario != base){
        		if(opList[[scenario]][[4]]$SensitivityGroup == sensitivityGroup){
        			sage <- baseRep$sage
        			rnyr <- length(baseRep$yr)
        			baseryr <- baseRep$yr[(1+sage):rnyr]
        			ryrtest <- opList[[scenario]][[4]]$yr
        			if(length(ryrtest) >length(baseRep$yr)) {
        				rnyr <- length(opList[[scenario]][[4]]$yr)
        				sensryr <- opList[[scenario]][[4]]$yr[(1+sage):rnyr]
        			} else sensryr <- baseryr
        			
        	}}}		
    	
    	
    mc <- baseRep$mc.rt/1000
    mc.rt <- as.data.frame(window(mcmc(mc),start=Burn,thin=Thin)) 
    rt <- apply(mc.rt,2,quantile,probs=c(0.025,0.5,0.975))
    if(useMaxYlim){
      yUpperLimit <- max(rt)
    }else{
      yUpperLimit <- ylimit
    }
    	 
   xp <- plot(baseryr,
               rt[2,],
               type="p",
               pch=20,
               col=color,
               xlim=c(min(sensryr),max(sensryr)),
               ylim=c(0,yUpperLimit),
               xlab="Year", 
               ylab="Recruits (millions)",
               main="Recruits",
               las=1)
    arrows(baseryr, rt[1, ],baseryr,rt[3,],code=3,angle=90,length=0.01,col=color)
    abline(h=median(as.matrix(mc.rt)),col=2,lty=2)
    abline(h=mean(as.matrix(mc.rt)),col=3,lty=2)
    currOffset <- offset
    
    for(scenario in 1:length(opList)){
      if(scenario != base){
        if(opList[[scenario]][[4]]$SensitivityGroup == sensitivityGroup){
                sage <-opList[[scenario]][[4]]$sage
        rnyr <- length(opList[[scenario]][[4]]$yr)
          ryr <- opList[[scenario]][[4]]$yr[(1+sage):rnyr]
         
          mc <- opList[[scenario]][[4]]$mc.rt/1000
          mc.rt <- as.data.frame(window(mcmc(mc),start=Burn,thin=Thin)) 
          rt <- apply(mc.rt,2,quantile,probs=c(0.025,0.5,0.975))
          color <- color + 1          
          colors <- c(colors,color)
          par(new=T)
           
          plot(ryr+currOffset,
               rt[2,],
               pch=20,
               #xlim=c(min(ryr),max(ryr)),
                xlim=c(min(sensryr),max(sensryr)),
               ylim=c(0,yUpperLimit),
               col=color,
               xlab="",
               ylab="",
               las=1,
               axes=F)
          arrows(ryr+currOffset, rt[1, ],ryr+currOffset,rt[3,],code=3,angle=90,length=0.01,col=color)
          par(new=F)
          currOffset <- currOffset + offset
          runNames <- c(runNames,strsplit(opList[[scenario]][[1]],"/")[[1]][3])
        }
      }
    }
    lty <- c(rep(1,(length(runNames))),2,2)
    runNames <- c(runNames,"base long-term median","base long-term mean")
    legend("topright",
           runNames,
           lty=lty,
           col=c(colors,2,3),
           bty="n",
           lwd=2) 
    filename <- paste("SensitivityGroup_",sensitivityGroup,"_Recruits",sep="")
    saveFig(filename)   

###########################
##BIOMASS  
  }else if(whichPlot == "biomass"){
    
  #Get the maximum year from the longest time series for xlim - i.e. might be comparing results from different lengths of data
    	#Need to do this here so the xlim is long enough
       	sensyr <- baseRep$yrs
      	 mcbt <- baseRep$mc.sbt/1000
	 post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
   	 MaxBt <- apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975))
         MaxBt <-max(MaxBt)
          
      	for(scenario in 1:length(opList)){
  	      if(scenario != base){
          		if(opList[[scenario]][[4]]$SensitivityGroup == sensitivityGroup){
          			
          			#Get max xlim
          			maxnyr <- length(baseRep$yrs)
          			yrtest <- opList[[scenario]][[4]]$yrs
          			if(length(yrtest) >maxnyr) {
          				maxnyr <- length(opList[[scenario]][[4]]$yrs)
          				sensyr <- opList[[scenario]][[4]]$yrs
          			} 
          			
          			#Now get max ylim
          			mcbt <- opList[[scenario]][[4]]$mc.sbt/1000
				 post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
          			ScBt<- apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975))
          			if(max(ScBt) > MaxBt) MaxBt <-max(ScBt)
          	}}}
  
    mcbo <- baseRep$mc$bo/1000
    post.bo <- as.data.frame(window(mcmc(mcbo),start=Burn,thin=Thin))
    boci <- apply(post.bo,2,quantile,probs=c(0.025,0.5,0.975))

    mcbt <- baseRep$mc.sbt/1000
    post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
    btci <- apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975))
    
    if(useMaxYlim){
      yUpperLimit <- 1.2*MaxBt
    }else{
      yUpperLimit <- ylimit
    }
    matplot(baseRep$yrs,
            t(btci),
            type="l",
            col=color,
            lty=c(2,1,2),
            lwd=2,
            ylim=c(0,yUpperLimit),
            xlim=c(min(sensyr),max(sensyr)),
           xlab="Year",
            ylab="Biomass",
            main="Biomass",
            las=1)
    
    # Shade the confidence interval
    xx <- c(baseRep$yrs,rev(baseRep$yrs))
    yy <- c(btci[1,],rev(btci[3,]))
    shade <- getShade(color,opacity)
    polygon(xx,yy,density=NA,col=shade)
    # End shade the confidence interval
    
    points(baseRep$yrs[1]-0.8,boci[2],col=color,pch=1)
    arrows(baseRep$yrs[1]-0.8,boci[1],baseRep$yrs[1]-0.8,boci[3],col=color, code=0, lwd=1.5)
    currOffset <- offset
    
    for(scenario in 1:length(opList)){
      if(scenario != base){
        if(opList[[scenario]][[4]]$SensitivityGroup == sensitivityGroup){
          mcbo <- opList[[scenario]][[4]]$mc$bo/1000
          post.bo <- as.data.frame(window(mcmc(mcbo),start=Burn,thin=Thin))
          boci <- apply(post.bo,2,quantile,probs=c(0.025,0.5,0.975))

          mcbt <- opList[[scenario]][[4]]$mc.sbt/1000
          post.bt <- as.data.frame(window(mcmc(mcbt),start=Burn,thin=Thin))
          btci <- apply(post.bt,2,quantile,probs=c(0.025,0.5,0.975))

          color <- color + 1          
          colors <- c(colors,color)
          par(new=T)
          
          matplot(opList[[scenario]][[4]]$yrs,
                  t(btci),
                  type="l",
                  col=color,
                  lty=c(2,1,2),
                  lwd=2,
                  xlim=c(min(sensyr),max(sensyr)),
                 ylim=c(0,yUpperLimit),
                  xlab="",
                  ylab="",
                  las=1)
          
          # Shade the confidence interval
          xx <- c(opList[[scenario]][[4]]$yrs,rev(opList[[scenario]][[4]]$yrs))
          yy <- c(btci[1,],rev(btci[3,]))
          shade <- getShade(color,opacity)
          polygon(xx,yy,density=NA,col=shade)
          # End shade the confidence interval

          par(new=F)
          points(opList[[scenario]][[4]]$yrs[1]-0.8+currOffset,boci[2],col=color,pch=1)
          arrows(opList[[scenario]][[4]]$yrs[1]-0.8+currOffset,boci[1],opList[[scenario]][[4]]$yrs[1]-0.8+currOffset,boci[3],col=color, code=0, lwd=1.5)
          currOffset <- currOffset + offset
          runNames <- c(runNames,strsplit(opList[[scenario]][[1]],"/")[[1]][3])
        }
      }
    }
    lty <- c(rep(1,(length(runNames))))
    legend("topright",runNames,lty=lty,col=colors,bty="n", lwd=2)
    
    filename <- paste("SensitivityGroup_",sensitivityGroup,"_Biomass",sep="")
    saveFig(filename)
###########################
##DEPLETION      
  }else if(whichPlot == "depletion"){
     #Get the maximum year from the longest time series for xlim - i.e. might be comparing results from different lengths of data
    	#Need to do this here so the xlim is long enough
   	 for(scenario in 1:length(opList)){
      	      if(scenario != base){
              		if(opList[[scenario]][[4]]$SensitivityGroup == sensitivityGroup){
              			
              			maxnyr <- length(baseRep$yrs)
              			yrtest <- opList[[scenario]][[4]]$yrs
              			if(length(yrtest) >maxnyr) {
              				maxnyr <- length(opList[[scenario]][[4]]$yrs)
              				sensyr <- opList[[scenario]][[4]]$yrs
              			} else sensyr <- baseRep$yrs
          	}}}
    
    
    mcdt <- baseRep$mc.sbdepletion
    post.dt  <- as.data.frame(window(mcmc(mcdt),start=Burn,thin=Thin))
    dtci <- apply(post.dt,2,quantile,probs=c(0.025,0.5,0.975))
    
    if(useMaxYlim){
      yUpperLimit <- 2*max(dtci)
    }else{
      yUpperLimit <- ylimit
    }
    matplot(baseRep$yrs,
            t(dtci),
            type="l",
            col=color,
            lty=c(2,1,2), 
            lwd=2,
            ylim=c(0,yUpperLimit),
             xlim=c(min(sensyr),max(sensyr)),
            xlab="Year",
            ylab="Depletion",
            main="Depletion")
    
    # Shade the confidence interval
    xx <- c(baseRep$yrs,rev(baseRep$yrs))
    yy <- c(dtci[1,],rev(dtci[3,]))
    shade <- getShade(color,opacity)
    polygon(xx,yy,density=NA,col=shade)
    # End shade the confidence interval
    
    abline(h=0.40, lwd=mtLineWidth, col=mtLineColor, lty=mtLineType)
    for(scenario in 1:length(opList)){
      if(scenario != base){
        if(opList[[scenario]][[4]]$SensitivityGroup == sensitivityGroup){
          mcdt <- opList[[scenario]][[4]]$mc.sbdepletion
          post.dt  <- as.data.frame(window(mcmc(mcdt),start=Burn,thin=Thin))
          dtci <- apply(post.dt,2,quantile,probs=c(0.025,0.5,0.975))
          color <- color + 1          
          colors <- c(colors,color)
          par(new=T)
          matplot(opList[[scenario]][[4]]$yrs,
                  t(dtci),
                  type="l",
                  col=color,
                  lty=c(2,1,2), 
                  lwd=2,
                  xlim=c(min(sensyr),max(sensyr)),
                  ylim=c(0,yUpperLimit),
                  xlab="",
                  ylab="")
          
          # Shade the confidence interval
          xx <- c(opList[[scenario]][[4]]$yrs,rev(opList[[scenario]][[4]]$yrs))
          yy <- c(dtci[1,],rev(dtci[3,]))
          shade <- getShade(color,opacity)
          polygon(xx,yy,density=NA,col=shade)
          # End shade the confidence interval

          par(new=F)
          runNames <- c(runNames,strsplit(opList[[scenario]][[1]],"/")[[1]][3])
          
        }
      }
    }
    lty <- c(rep(1,(length(runNames))),mtLineType)
    lwd <- c(rep(2,(length(runNames))),mtLineWidth)
    colors <- c(colors,mtLineColor)
    runNames <- c(runNames,"Base 0.4 B0")
    legend("topright",runNames,lty=lty,col=colors,bty="n", lwd=lwd)
    filename <- paste("SensitivityGroup_",sensitivityGroup,"_Depletion",sep="")
    saveFig(filename)
   
  ###########################
      ##REFERENCE POINTS BOXPLOTS  
  }else if(whichPlot == "refpts"){
     
    
    mcbo <- baseRep$mc$bo/1000
    mcbmsy <- baseRep$mc$bmsy/1000
    mcmsy <- baseRep$mc$msy/1000
    mcfmsy <- baseRep$mc$fmsy
    
   post.bo <- as.vector(window(mcmc(mcbo),start=Burn,thin=Thin))
   post.bmsy <- as.vector(window(mcmc(mcbmsy),start=Burn,thin=Thin))
    post.msy <- as.vector(window(mcmc(mcmsy),start=Burn,thin=Thin))
   post.fmsy <- as.vector(window(mcmc(mcfmsy),start=Burn,thin=Thin))
   boxCols <- color
           
    for(scenario in 1:length(opList)){
      if(scenario == base){
          runNames <- strsplit(opList[[scenario]][[1]],"/")[[1]][3]
           AxisName="Base"
          print(runNames)
        }
      if(scenario != base){
        if(opList[[scenario]][[4]]$SensitivityGroup == sensitivityGroup){
          	
              
              color <- boxCols + 1          
              boxCols <- c(colors,color)
              mcbo <- opList[[scenario]][[4]]$mc$bo/1000
	      mcbmsy <- opList[[scenario]][[4]]$mc$bmsy/1000
	      mcmsy <- opList[[scenario]][[4]]$mc$msy/1000
	      mcfmsy <- opList[[scenario]][[4]]$mc$fmsy
	      
	      post.bo2 <- as.vector(window(mcmc(mcbo),start=Burn,thin=Thin))
	      post.bmsy2 <- as.vector(window(mcmc(mcbmsy),start=Burn,thin=Thin))
	      post.msy2 <- as.vector(window(mcmc(mcmsy),start=Burn,thin=Thin))
   	      post.fmsy2 <- as.vector(window(mcmc(mcfmsy),start=Burn,thin=Thin))
   	      
   	       post.bo <- cbind(post.bo,post.bo2)
   	       post.bmsy <- cbind(post.bmsy,post.bmsy2)
   	       post.msy <- cbind(post.msy,post.msy2)
   	       post.fmsy <- cbind(post.fmsy,post.fmsy2)
   	       
   	       runNamesi <-strsplit(opList[[scenario]][[1]],"/")[[1]][3]
   	      runNames <- c(runNames, runNamesi)
              AxisName <- c(AxisName,substr(runNamesi,1,2))
              
        }
      }
    }

     #Set limits for MSY plot
     if(useMaxYlim){
          yUpperLimit <- max(post.msy)
        }else{
          yUpperLimit <- 0.1*ylimit	#set ylimit high on GUI and assume that MSY will be smaller than Bo or Bmsy
    }

     par(mfrow=c(2,2), mai=c(0.3,0.5,0.4,0.2), oma=c(1.,1.2,0.2,0.1))
    boxplot( post.fmsy, pch=".", range=0.95, col=boxCols, names=AxisName, main="FMSY (/y)", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,1))	 #1.1*max(post.fmsy)
    boxplot( post.msy, pch=".", range=0.95 , col=boxCols, names=AxisName, main="MSY (1000 t)", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,yUpperLimit))
    legend("topleft",runNames,lty=1, col=boxCols,bty="n", lwd=2)
     #Set limits for Bo plot
     if(useMaxYlim){
              yUpperLimit <- max(post.bo)
            }else{
              yUpperLimit <- ylimit
    }
    boxplot( post.bo, pch=".", range=0.95,  col=boxCols, names=AxisName, main="B0 (1000 t)", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,yUpperLimit))
     #Set limits for Bmsy plot
     if(useMaxYlim){
              yUpperLimit <- max(post.bmsy)
            }else{
              yUpperLimit <- 0.5*ylimit	#assume Bmsy smaller than Bo
    }
    boxplot( post.bmsy, pch=".", range=0.95, col=boxCols, names=AxisName, main="BMSY (1000 t)", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,yUpperLimit))
     
    filename <- paste("SensitivityGroup_",sensitivityGroup,"_RefPts",sep="")
    saveFig(filename)
	}
	par(op)
}
