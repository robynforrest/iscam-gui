#**********************************************************************************
# iscam-gui-figures-catch.r
# This file contains the code for catch values iscam inputs using the
# infrastructure provided with iscam-gui.
#
# Author            : Chris Grandin
# Development Date  : October 2013 - Present
#**********************************************************************************

plotCatch <- function(scenario   = 1,         # Scenario number
                      plotNum    = 1,         # Plot code number
                      savefig    = .SAVEFIG,  # TRUE/FALSE for plot output
                      fileText   = "Default", # Name of the file if png==TRUE
                      plotMCMC   = FALSE,     # TRUE/FALSE to plot MCMC output
                      ci         = NULL,      # confidence interval in % (0-100)
                      multiple   = FALSE,     # TRUE/FALSE to plot sensitivity cases
                      sensGroup  = 1,         # Sensitivity group to plot if multiple==TRUE
                      index      = 1,         # Survey index to plot if plotNum==7
                      # PlotSpecs: Width, height, and resolution of screen and file
                      ps         = list(pngres = .RESOLUTION,
                                        pngw   = .WIDTH,
                                        pngh   = .HEIGHT,
                                        res    = .RESOLUTION,
                                        w      = .WIDTH,
                                        h      = .HEIGHT),
                      leg        = "topright",   # Legend location. If NULL, none will be drawn
                      figtype    = .FIGURE_TYPE, # The filetype of the figure with period, e.g. ".png"
                      showtitle  = TRUE,         # Show the main title on the plot
                      units      = .UNITS,
                      silent     = .SILENT){

  # Assumes that 'op' list exists and has been populated correctly.
  # plotNum must be one of:
  # 1  Landings
  # 2
  # 3  Catch fit
  currFuncName <- getCurrFunc()

  if(plotNum < 1 || plotNum > 4){
    cat0(.PROJECT_NAME,"->",currFuncName,"The plotNum must be between 1 and 10. You passed ",plotNum)
    return(FALSE)
  }

  scenarioName <- op[[scenario]]$names$scenario
  figDir       <- op[[scenario]]$names$figDir
  res          <- ps$pngres
  width        <- ps$pngw
  height       <- ps$pngh
  resScreen    <- ps$res
  widthScreen  <- ps$w
  heightScreen <- ps$h

  if(multiple){
    filenameRaw  <- paste0("SensitivityGroup_",sensGroup,"_",fileText,figtype)
    filename     <- file.path(.SENS_FIGURES_DIR_NAME,filenameRaw)
    # Extract models in the current sensitivity group
    if(is.null(sens[[sensGroup]])){
      cat0(.PROJECT_NAME,"->",currFuncName,"The sensitivity group you selected has no members.")
      return(NULL)
    }
    models <- sens[[sensGroup]]
  }else{
    filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,figtype)
    filename     <- file.path(figDir,filenameRaw)
    models <- scenario # For the non-multiple case
  }
  if(plotMCMC){
    # Remove models which do not have MCMC outputs
    type <- "mcmc"
    validModels <- getValidModelsList(models, type = type)
  }else{
    type <- "mpd"
    validModels <- getValidModelsList(models, type = type)
  }

  out    <- validModels[[1]]
  colors <- validModels[[2]]
  names  <- validModels[[3]]
  inputs <- validModels[[4]]
  linetypes <- validModels[[5]]

  if(is.null(validModels)){
    if(is.null(names)){
      cat0(.PROJECT_NAME,"->",currFuncName,"The model ",scenarioName," has no ",type," output associated with it.\n")
    }else{
      cat0(.PROJECT_NAME,"->",currFuncName,"The model ",names[[1]]," has no ",type," output associated with it.\n")
    }
    return(NULL)
  }

 if(savefig){
    graphics.off()
    if(figtype == .PNG_TYPE){
      png(filename,res=res,width=width,height=height,units=units)
    }
    if(figtype == .EPS_TYPE){
      setEPS(horizontal=FALSE, onefile=FALSE, paper="special",width=width,height=height)
      postscript(filename)
    }
  }else{
    windows(width=widthScreen,height=heightScreen)
  }

  if(plotNum == 1){
    plotCatches(inp = inputs[[1]], scenarioName, leg = leg, col = color, showtitle = showtitle)
  }
  if(plotNum == 2){
    plotSPR(inp = inputs, scenarioName, leg = leg, col = color, showtitle = showtitle)
  }
  if(plotNum == 3){
    if(plotMCMC){
      cat0(.PROJECT_NAME,"->",currFuncName,"MCMC plots for Catch fits not implemented.")
    }else{
      plotCatchFit(inputs, out, colors=colors, lty=linetypes, names=names, scenarioName=scenarioName, leg = leg, showtitle = showtitle)
    }
  }
  if(plotNum == 4){
    plotExpVsObsAnnualMeanWt(inp = inputs, out=out, scenarioName, leg = leg, col = color, showtitle = showtitle)
  }

  if(savefig){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

plotCatches <- function(inp,
                        scenarioName,
                        verbose = FALSE,
                        leg = "topright",
                        showtitle = TRUE,
                        col = 1){
  # Catch plot for iscam model, plots by gear
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  catch <- as.data.frame(inp$catch)
  p <- ggplot(catch,aes(x=factor(year),value,fill=factor(gear)))
	p <- p + geom_bar(width=0.75,position="dodge",stat="identity")
  p <- p + labs(x="Year",y="Catch (1000 mt)",fill="Gear")
  p <- p + .PLOT_THEME
  p <- p + theme(axis.text.x = element_text(angle = -90, hjust = 0))
	print(p)
}

plotSPR <-  function(inp,
                     scenarioName,
                     verbose = FALSE,
                     leg = "topright",
                        showtitle = TRUE,
                     col = 1){

}

plotCatchFit<-function(inp     = NULL,
                       out     = NULL,
                       colors  = NULL,
                       names   = NULL,
                       lty     = NULL,
                       scenarioName,
                       verbose = FALSE,
                       leg = "topright",
                       showtitle = TRUE){
  # Catch fits plot, out contains a list of models to plot,
  # it must be at least length 1.
  # Assumes only one catch gear
  currFuncName <- getCurrFunc()
  if(is.null(inp)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an input vector (inp).")
    return(NULL)
  }
  if(is.null(out)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an output vector (out).")
    return(NULL)
  }
  if(length(out) < 1){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply at least one element in the output vector (out).")
    return(NULL)
  }
  if(is.null(colors)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a colors vector (colors).")
    return(NULL)
  }
  if(is.null(names)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a names vector (names).")
    return(NULL)
  }
  if(is.null(lty)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a linetypes vector (lty).")
    return(NULL)
  }

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  catchData <- as.data.frame(inp[[1]]$catch)
  years <- catchData$year
  obsCt <- catchData$value
  predCt <- out[[1]]$mpd$ct
  yUpper <- max(obsCt, predCt)

  for(model in 1:length(out)){
    yUpper <- max(yUpper, out[[model]]$mpd$ct)
  }

  xlim <- range(years)
  ylim <- c(0,yUpper)
  titletext <- ""
  if(showtitle){
    titletext <- "Catch fit"
  }
  plot(years, obsCt, pch=19, col=colors[[1]], lty=lty[[1]], xlim=xlim, ylim=ylim, type="p", xlab="Year", ylab="Catch (1000 mt)", main=titletext)
  lines(years, predCt, col=colors[[1]], lty=lty[[1]])
  if(length(out) > 1){
    for(line in 2:length(out)){
      predCt <- out[[line]]$mpd$ct
      lines(years, predCt, col=colors[[line]], lty=lty[[line]])
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }

}

plotExpVsObsAnnualMeanWt<-function(inp,
                                out,
                                scenarioName,
                                verbose = FALSE,
                                leg = "topright",
                                showtitle = TRUE,
                                col = 1){
  nmeanwtObs <- inp$data$nmeanwtobs
  if( nmeanwtObs > 0){
		  meanwtData <- inp$data$meanwtdata
		  years <- meanwtData[,"year"]
		  obsMeanWt <- meanwtData[,"meanwt"]
		  gear <-meanwtData[,"gear"]
		  gearList<-unique(gear)
		  ngear<-length(gearList)	 #only plot for gears with data
		  predMeanWt <-out$annual_mean_weight

		  if (ngear==1) par(mfrow=c(1,1),mar=c(5,4,2,2))
		  if (ngear == 2) par(mfrow=c(2,1),mar=c(4,4,2,2))
		  if (ngear == 3 | ngear == 4) par(mfrow=c(2,2),mar=c(3,3,2,2))
		  if (ngear == 5 | ngear == 6) par(mfrow=c(3,2),mar=c(2,2,2,2))

		  for (i in 1:ngear) {
		      # Set-up plot area
		      xLim <- range(years)
		      yLim <- c(0,(max(obsMeanWt[gear==gearList[i]],predMeanWt[gear==gearList[i]])*1.1))

		      plot(xLim, yLim, type="n", axes=TRUE, xlab="Year", ylab="Mean Weight in Catch")

		      points(years[gear==gearList[i]], obsMeanWt[gear==gearList[i]], pch=19)
		      lines(years[gear==gearList[i]], predMeanWt[gear==gearList[i]], col="red")
		      box()
		  }
		  par(mfrow=c(1,1),mar=c(5,4,2,2))
	}else{
    cat0("WARNING: No Annual Mean Weight Data")
  }
  
}
