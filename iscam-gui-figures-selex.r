#**********************************************************************************
# ss-explore-figures-selex.r
# This file contains the code for plotting selectivity values SS outputs using the
# infrastructure provided with ss-explore.
#
# Author            : Chris Grandin
# Development Date  : October 2013
# Current version   : 1.0
#**********************************************************************************

plotSelex <- function(plotNum    = 1,         # Plot code number
                      compFitSex = 1,         # sex to plot (1=M/Both, 2=F)
                      savefig    = .SAVEFIG,  # TRUE/FALSE for PNG image output
                      fileText   = "Default", # Name of the file if png==TRUE
                      plotMCMC   = FALSE,     # TRUE/FALSE to plot MCMC output
                      ci         = NULL,      # confidence interval in % (0-100)
                      multiple   = FALSE,     # TRUE/FALSE to plot sensitivity cases
                      sensGroup  = 1,         # Sensitivity group to plot if multiple==TRUE
                      index      = 1,         # Gear index to plot
                      figtype    = .FIGURE_TYPE # The filetype of the figure with period, e.g. ".png"
                      ){

  # plotNum must be one of:
  # 1 logistic selectivity - age or length based will be detected automatically
 	# 1  Length-based selectivity in end year by fleet
  # 2  Age-based selectivity in end year by fleet
  # 3  Selectivity at length time-varying surface
  # 4  Selectivity at length time-varying contour
  # 5  Retention at length time-varying surface
  # 6  Retention at length time-varying surface
  # 7  Discard mortality time-varying surface
  # 8  Discard mortality time-varying contour
  # 9  Selectivity, retention, and discard mortality at length in ending year
  # 10 NOT USED
  # 11 Selectivity at age time-varying surface
  # 12 Selectivity at age time-varying contour
  # 13 Selectivity at age in ending year if time-varying
  # 14 Selectivity at age in ending year if NOT time-varying
  # 15 NOT USED
  # 16 NOT USED
  # 17 NOT USED
  # 18 NOT USED
  # 19 NOT USED
  # 20 NOT USED
  # 21 Selectivity at age and length contour with overlaid growth curve
  # 22 Selectivity with uncertainty if requested at end of control file

  if(plotNum < 1   ||
     plotNum > 22  ||
     plotNum == 10 ||
     plotNum == 15 ||
     plotNum == 16 ||
     plotNum == 17 ||
     plotNum == 18 ||
     plotNum == 19 ||
     plotNum == 20
     ){
    return(FALSE)
  }
  val      <- getWinVal()
  scenario <- val$entryScenario
  currFuncName <- getCurrFunc()
  isMCMC   <- op[[scenario]]$inputs$log$isMCMC
  figDir   <- op[[scenario]]$names$figDir
  out      <- op[[scenario]]$outputs$mpd
  res          <- val$entryResolution
  width        <- val$entryWidth
  height       <- val$entryHeight
  resScreen    <- val$entryResolutionScreen
  widthScreen  <- val$entryWidthScreen
  heightScreen <- val$entryHeightScreen

  filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,figtype)
  filename     <- file.path(figDir,filenameRaw)
  if(savefig){
    graphics.off()
    if(figtype == .PNG_TYPE){
      png(filename,res=res,width=width,height=height,units=units)
    }
    if(figtype == .EPS_TYPE){
      postscript(filename, horizontal=FALSE, paper="special",width=width,height=height)
    }
  }else{
    windows(width=widthScreen,height=heightScreen)
  }
  if(isMCMC){
   # plot mcmc model runs
  }else{
    # plot mpd model runs
  }

  if(plotNum==1)  plotLogisticSel(scenario, index, compFitSex)
  if(plotNum>=2)  cat("No Plot Yet -- Coming Soon!!\n")

  if(savefig){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

plotLogisticSel	<-	function(scenario, index, sex){
  # Currently only implemented for seltypes 1,6 and 11 (estimated logistic age-based, fixed logistic age-based, or estimated logistic length-based)

  currFuncName <- getCurrFunc()
  if(sex == 1){
    sexstr <- "Male"
  }else if(sex == 2){
    sexstr <- "Female"
  }else{
    sexstr <- "Combined sexes"
  }

  gearNames <- op[[scenario]]$inputs$data$gearNames
  if(op[[scenario]]$inputs$data$hasAgeGearNames){
    gearTitle <- paste0(gearNames[index]," - ",sexstr)
  }else{
    gearTitle <- paste0("Gear ",gearNames[index]," - ",sexstr)
  }
  aflag <- 0 #flag to set age or length
 	ngear <-  op[[scenario]]$inputs$data$ngear
	if(index <= ngear){
		selType <-   op[[scenario]]$inputs$control$sel[1,index]
		selBlocks <- op[[scenario]]$inputs$control$sel[10,index] #selectivity time blocks
		if(selType==1 || selType ==6){
      aflag <-1
    }else if(selType==11){
      aflag <- 2
    }

		Age <- op[[scenario]]$output$mpd$age
		Len <- op[[scenario]]$output$mpd$la

		#no plot if sel is not one of the types listed above
		if(aflag > 0){
			logselData <-  op[[scenario]]$output$mpd$log_sel
			logselData <- logselData[which(logselData[,1]==index),]
			xx <- logselData[,4:ncol(logselData)]

			selData <- exp(xx)
			selData <- selData[1,] #selectivity in first block
			startBlocks <- op[[scenario]]$inputs$control$syrtimeblock[index,]
			legtext <- paste("Selectivity Block 1 :", startBlocks[1])

			selData <- as.matrix(selData)

			if(selBlocks>1){
				for(i in 2:selBlocks){
					logselBlockData <- xx[which(logselData[,3]==startBlocks[i]),]
					selData <- cbind(selData, exp(logselBlockData))
				}
			}
      if(aflag==1){
        Xlab <- "Age"
        plot(Age, selData[,1], type="l", xlab=Xlab, ylab="Proportion", lwd=2, col=1, las=1, main=gearTitle, ylim=c(0,1.1))
        if(selBlocks>1){
          for(i in 2:selBlocks) {
            lines(Age, selData[,i], lty=i, col=i, lwd=2)
            legtext <- c(legtext, paste("Selectivity Block",i,":", startBlocks[i]))
          }
        }
        legend("topleft", legend=legtext, lty=1:selBlocks, col=1:selBlocks, lwd=2, bty="n")
      }
      if(aflag==2){
        Xlab <- "Length-at-Age"
        plot(Len, selData[,1], type="l", xlab=Xlab, ylab="Proportion", lwd=2, col=1, las=1,  main=gearTitle, ylim=c(0,1.1))
        if(selBlocks>1){
					for(i in 2:selBlocks){
						lines(Len, selData[,i], lty=i, col=i, lwd=2)
						legtext <- c(legtext, paste("Selectivity Block",i,":", startBlocks[i]))
					}
				}
        legend("topleft", legend=legtext, lty=1:selBlocks, col=1:selBlocks, lwd=2, bty="n")
			}
    }else{
      cat0(.PROJECT_NAME,"->",currFuncName,"Plot not currently implemented for the type of selectivity used for this gear.")
    }
  }else{
    cat0(.PROJECT_NAME,"->",currFuncName,"Gear index exceeds the number of gears. Choose gear between 1 and ", ngear,".")
  }
}
