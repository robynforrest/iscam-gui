#**********************************************************************************
# ss-explore-figures-biology.r
# This file contains the code for plotting biological values SS outputs using the
# infrastructure provided with ss-explore.
#
# Author            : Chris Grandin
# Development Date  : August/September 2013
# Current version   : 1.0
#**********************************************************************************

plotBiology <- function(plotNum    = 1,         # Plot code number
                   png        = .PNG,      # TRUE/FALSE for PNG image output
                   fileText   = "Default", # Name of the file if png==TRUE
                   plotMCMC   = FALSE,     # TRUE/FALSE to plot MCMC output
                   ci         = NULL,      # confidence interval in % (0-100)
                   multiple   = FALSE,     # TRUE/FALSE to plot sensitivity cases
                   sensGroup  = 1,         # Sensitivity group to plot if multiple==TRUE
                   index      = 1         # Gear index to plot if plotNum==11,12 or 13
                   ){

  # plotNum must be one of:
  # 1  Mean weight at length for last year
  # 2  Maturity at age
  # 3  Fecundity (only plotted if  all elements of biology$Fecundity are not NA)
  # 4  Fecundity as a function of weight (not always available)
  # 5  Fecundity as a function of length (not always available)
  # 6  Spawning output at length
  # 7  Ending year expected growth, length at age
  # 8  Time-varying natural mortality (only plotted if growthdatF$M vector contains different values)
  # 9  Perspective plot of time-varying growth (only plotted if growthvaries and growthseries are non-NULL)
  # 10 Contour plot of time-varying growth (only plotted if growthvaries and growthseries are non-NULL)
  # 11 Bubble plot of composition data
  # 12 Bar plot of composition fits
  # 13 Bubble plot of composition residuals

  if(plotNum < 1 || plotNum > 13){
    return(FALSE)
  }
  val          <- getWinVal()
  currFuncName <- getCurrFunc()
  scenario     <- val$entryScenario
  isMCMC       <- op[[scenario]]$inputs$log$isMCMC
  figDir       <- op[[scenario]]$names$figDir
  out          <- op[[scenario]]$outputs$mpd
  outMCMC      <- op[[scenario]]$outputs$mcmc
  filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,".png")
  filename     <- file.path(figDir,filenameRaw)
  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)

  }
  #SSplotBiology(out,
  #              plot     = TRUE,
  #              print    = FALSE,
  #              subplots = plotNum,
  #              pheight  = height,
   #             pwidth   = width,
   #             punits   = units,
   #             res      = res,
   #             verbose  =!silent)
  
  
  if(plotNum==1)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==2)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==3)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==4)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==5)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==6)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==7)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==8)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==9)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==10)  cat("No Plot Yet -- Coming Soon!!\n")
  
  
  #composition data
   if(plotNum==11) plotComposition(scenario, index)
   if(plotNum==12) plotCompositionFit(scenario, index)
   if(plotNum==13) plotCompositionResid(scenario, index)
  
  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}


#Functions
 plotComposition	<-	function(scenario, index){
 	nAgears <-  op[[scenario]]$input$data$nagears
 	nAgearsobs <- op[[scenario]]$input$data$nagearsvec
	Flags <-   op[[scenario]]$input$data$agecompflag  #0 = length data 1= age data
	
 	
 	if(nAgearsobs[1] > 0){
 		compData <-  as.data.frame(op[[scenario]]$output$mpd$d3_A ) #Get the composition data	   
		gears <- unique(compData[,2])
		
		if(is.element(index, gears)){
			#Get the index for the gear associated with the index number so the correct sage and nage can be extracted
			#For example, if the two gears with data are 1 and 3, when the user selects index 3 on the GUI, sage and nage are the SECOND elements of n_A_sage and n_A_nage
			gearindex <- which(gears==index) 
			sage <- op[[scenario]]$output$mpd$n_A_sage[gearindex]	    #Need to match the gear number to the correct element of 	n_A_sage
			nage <- op[[scenario]]$output$mpd$n_A_nage[gearindex]	    #Need to match the gear number to the correct element of 	n_A_nage
			flag <- Flags[gearindex]
			if(flag==0) Ylab="Length"
		        if(flag==1) Ylab="Age"
			
			compData <- compData[which(compData[,2]==index) ,]   #Get only the composition data for the current index
			syr <- compData[1,1]
			nyr <- compData[length(compData[,1]),1]
			compData <- compData[, 6:ncol(compData)]

			Prop <- matrix(nrow=nrow(compData), ncol=ncol(compData))
                       		for(ii in 1:nrow(compData)) Prop[ii,] <-  as.numeric(compData[ii,]/sum(compData[ii,]) )

			plotBubbles(t(Prop), xval=syr:nyr,yval=sage:nage, prettyaxis=T, size=0.1,powr=0.5,xlab="Year",ylab=Ylab,main=paste("Gear", index), las=1)
			legend("topleft", legend=c("Positive", "Zero"), col=c("black","blue"), pch=1, bty="n", cex=1.25)

			#bubble.plot(syr:nyr,sage:nage,Prop,scale=0.3,xlab="Year",ylab=Ylab,add=F,log.scale=T, main=paste("Gear", index), las=1)
 	         }else cat("WARNING: No composition data for this gear\n") 
   	}else cat("WARNING: No composition data for this scenario\n")  
}

 plotCompositionFit	<-	function(scenario, index){
 	nAgears <-  op[[scenario]]$input$data$nagears
 	nAgearsobs <- op[[scenario]]$input$data$nagearsvec
	Flags <-   op[[scenario]]$input$data$agecompflag  #0 = length data 1= age data
	
  	if(nAgearsobs[1] > 0){
 		compData <-  as.data.frame(op[[scenario]]$output$mpd$d3_A ) #Get the composition data - need this because there is no gear information with A_hat  
		fitData  <-  as.data.frame(op[[scenario]]$output$mpd$A_hat)  #Get the fitted data
		gears <- unique(compData[,2])
		
		if(is.element(index, gears)){
			#Get the index for the gear associated with the index number so the correct sage and nage can be extracted
			#For example, if the two gears with data are 1 and 3, when the user selects index 3 on the GUI, sage and nage are the SECOND elements of n_A_sage and n_A_nage
			gearindex <- which(gears==index) 
			sage <- op[[scenario]]$output$mpd$n_A_sage[gearindex]	    #Need to match the gear number to the correct element of 	n_A_sage
			nage <- op[[scenario]]$output$mpd$n_A_nage[gearindex]	    #Need to match the gear number to the correct element of 	n_A_nage
			flag <- Flags[gearindex]
			if(flag==0) Ylab="Length"
		        if(flag==1) Ylab="Age"
			
			xx <- as.data.frame(compData[which(compData[,2]==index) ,] )  #Get only the composition data for the current index
			syr <- xx[1,1]
			nyr <- xx[nrow(xx),1]
			yrs <- xx[,1]
			iyr<-length(yrs)
                        			
			fitData <- fitData[which(compData[,2]==index) ,]   #Use the composition dataframe to get the right rowsfor the fitted data (i.e.,for the current index)
		      	compData <- compData[, 6:ncol(compData)]
			Prop <- matrix(nrow=nrow(compData), ncol=ncol(compData))
                       		for(ii in 1:nrow(compData)) Prop[ii,] <-  as.numeric(compData[ii,]/sum(compData[ii,]) )

		       #Set the number of panels per plot   - no more than 16 per page
		        if(nrow(Prop) < 17) par(mfrow=c(4,4), oma=c(2,3,1,1), mai=c(0.3,0.3,0.3,0.2)) 
		       if(nrow(Prop) < 5)   par(mfrow=c(2,2), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2))
			
			#Set the number of graphs
			if(nrow(Prop) >= 17) par(mfrow=c(4,4), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2))  
			
			#counters
			ii <- 1 #plot counter
						
			for(i in 1:iyr){
			   year <- yrs[i]
			   obs <-  Prop[i,]
			   est <-   fitData[i,]

			   plot(sage:nage, obs, type="h", xlab="", ylab="", main=paste(year), las=1, ylim=c(0,max(rbind(obs,est))))
			   lines(sage:nage, est, lty=1, lwd=2, col=2)

			   ii <- ii+1
			  if(ii == 16){
				mtext(paste(Ylab), side=1, line=0.5, cex=1.3, outer=T)
				mtext("Proportion", side=2, line=0.6, cex=1.3, outer=T)
				mtext(paste("Gear", index), side=3, line=-0.5, cex=1.3, outer=T)
                                windows()
				 par(mfrow=c(4,4), oma=c(2,3,1,1), mai=c(0.3,0.3,0.3,0.2))  
				ii <- 1 # re-set
                      	   }#end if

			    if(i == iyr){
				mtext(paste(Ylab), side=1, line=0.5, cex=1.3, outer=T)
				mtext("Proportion", side=2, line=0.6, cex=1.3, outer=T)
				mtext(paste("Gear", index), side=3, line=-0.5, cex=1.3, outer=T)
			  }#end if
			} #end for i
 	         }else cat("WARNING: No composition data for this gear\n") 
   	}else cat("WARNING: No composition data for this scenario\n")  
 }




 plotCompositionResid	<-	function(scenario, index){
 	nAgears <-  op[[scenario]]$input$data$nagears
 	nAgearsobs <- op[[scenario]]$input$data$nagearsvec
	Flags <-   op[[scenario]]$input$data$agecompflag  #0 = length data 1= age data
	
 	
 	if(nAgearsobs[1] > 0){
 		compData <-  as.data.frame(op[[scenario]]$output$mpd$d3_A ) #Get the composition data - need this because there is no gear information with the residuals  
		residData  <-  as.data.frame(op[[scenario]]$output$mpd$A_nu)  #Get the residual data
		gears <- unique(compData[,2])
		
		if(is.element(index, gears)){
			#Get the index for the gear associated with the index number so the correct sage and nage can be extracted
			#For example, if the two gears with data are 1 and 3, when the user selects index 3 on the GUI, sage and nage are the SECOND elements of n_A_sage and n_A_nage
			gearindex <- which(gears==index) 
			sage <- op[[scenario]]$output$mpd$n_A_sage[gearindex]	    #Need to match the gear number to the correct element of 	n_A_sage
			nage <- op[[scenario]]$output$mpd$n_A_nage[gearindex]	    #Need to match the gear number to the correct element of 	n_A_nage
			flag <- Flags[gearindex]
			if(flag==0) Ylab="Length"
		        if(flag==1) Ylab="Age"
			
			xx <- as.data.frame(compData[which(compData[,2]==index) ,] )  #Get only the composition data for the current index
			syr <- xx[1,1]
			nyr <- xx[nrow(xx),1]
			residData <- residData[which(compData[,2]==index) ,]   #Use the composition dataframe to get the right rowsfor the residuals (i.e.,for the current index)
		      	
		      	plotBubbles(t(residData), xval=syr:nyr,yval=sage:nage, prettyaxis=T, size=0.1,powr=0.5,xlab="Year",ylab=Ylab,main=paste("Gear", index), las=1)
			legend("topleft", legend=c("Positive", "Negative", "Zero"), col=c("black","red","blue"), pch=1, bty="n", cex=1.25)


 	         }else cat("WARNING: No composition data for this gear\n") 
   	}else cat("WARNING: No composition data for this scenario\n")  
}


