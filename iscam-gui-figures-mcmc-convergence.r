#**********************************************************************************
# iscam-gui-figures-mcmc-convergence.r
# This file contains the code to plot mcmc convergence information
#
# Author            : Chris Grandin
# Development Date  : April 2014 - Present
#**********************************************************************************

plotConvergence <- function(plotNum         = 1,
                            png             = .PNG,
                            fileText        = "Default",
                            exFactor        = 1.5,
                            showEntirePrior = TRUE,
                            units           = .UNITS,
                            silent          = .SILENT){

  # Plot a convergence plot for an MCMC model run
  # plotNum must be one of:
  # 1 Trace plots

  currFuncName <- getCurrFunc()
  val          <- getWinVal()
  scenario     <- val$entryScenario
  scenarioName <- op[[scenario]]$names$scenario
  sensGroup    <- val$entrySensitivityGroup
  mcmcOut      <- op[[scenario]]$outputs$mcmc
  if(is.null(mcmcOut)){
    cat0(.PROJECT_NAME,"->",currFuncName,"The model ",scenarioName," has no mcmc output data associated with it.\n")
    return(NULL)
  }

  figDir       <- op[[scenario]]$names$figDir
  res          <- val$entryResolution
  width        <- val$entryWidth
  height       <- val$entryHeight
  resScreen    <- val$entryResolutionScreen
  widthScreen  <- val$entryWidthScreen
  heightScreen <- val$entryHeightScreen
  if(val$legendLoc == "sLegendTopright"){
    legendLoc <- "topright"
  }
  if(val$legendLoc == "sLegendTopleft"){
    legendLoc <- "topleft"
  }
  if(val$legendLoc == "sLegendBotright"){
    legendLoc <- "bottomright"
  }
  if(val$legendLoc == "sLegendBotleft"){
    legendLoc <- "bottomleft"
  }
  if(val$legendLoc == "sLegendNone"){
    legendLoc <- NULL
  }
  filenameRaw  <- paste0(scenarioName,"_",fileText,".png")
  filename     <- file.path(figDir,filenameRaw)
  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)
  }else{
    windows(width=widthScreen,height=heightScreen)
  }
  mcmcData <- mcmcOut$params
  if(plotNum == 1){
    plotTraces(mcmcData)
  }
  if(plotNum == 2){
    plotAutocor(mcmcData)
  }
  if(plotNum == 3){
    plotDensity(mcmcData)
  }
  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

plotTraces <- function(mcmcData = NULL, axis.lab.freq=200){
  # Traceplots for an mcmc matrix, mcmcData
  # axis.lab.freq is the frequency of x-axis labelling

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  val          <- getWinVal()
  burnin       <- val$burn
  thinning     <- val$thin
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  numParams <- ncol(mcmcData)
  # The next line is a simple algorithm, just generate the smallest square grid
  # that will hold all of the parameter trace plots.
  nrows <- ncols <- ceiling(sqrt(numParams))
	par(mfrow=c(nrows, ncols), las=1)
  mcmcData <- window(as.matrix(mcmcData), start=burnin, thin=thinning)
  for(param in 1:numParams){
    par(mar=.MCMC_MARGINS)
    mcmcTrace <- as.matrix(mcmcData[,param])
    plot(mcmcTrace, main=colnames(mcmcData)[param], type="l",ylab="",xlab="",axes=F)
    box()
    at <- labels <- seq(0,nrow(mcmcData), axis.lab.freq)
    axis(1, at=at, labels=labels)
    axis(2)
  }
}

plotAutocor <- function(mcmcData = NULL,
                        lag = c(0, 1, 5, 10, 15, 20, 30, 40, 50)){
  # Plot autocorrelations for all mcmc parameters
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  val          <- getWinVal()
  burnin       <- val$burn
  thinning     <- val$thin
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  numParams <- ncol(mcmcData)
  # The next line is a simple algorithm, just generate the smallest square grid
  # that will hold all of the parameter trace plots.
  nrows <- ncols <- ceiling(sqrt(numParams))
	par(mfrow=c(nrows, ncols), las=1)

  for(param in 1:numParams){
    par(mar=.MCMC_MARGINS)
    mcmcAutocor <- window(mcmc(as.ts(mcmcData[,param])), start = burnin, thin = thinning)
    autocorr.plot(mcmcAutocor, lag.max = 100, main = colnames(mcmcData)[param], auto.layout = FALSE)
  }
}

plotDensity <- function(mcmcData = NULL, color = 1, opacity = 30){
  # Plot densities for the mcmc parameters in the matrix mcmcData
	oldPar	<- par(no.readonly=T)
  on.exit(par(oldPar))

  val          <- getWinVal()
  burnin       <- val$burn
  thinning     <- val$thin
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  numParams <- ncol(mcmcData)
  # The next line is a simple algorithm, just generate the smallest square grid
  # that will hold all of the parameter trace plots.
  nrows <- ncols <- ceiling(sqrt(numParams))
	par(mfrow=c(nrows, ncols), las=1)

  for(param in 1:numParams){
    par(mar=.MCMC_MARGINS)
    dat <- window(mcmc(as.ts(mcmcData[,param])), start = burnin, thin = thinning)
    dens <- density(dat)
    plot(dens, main = colnames(mcmcData)[param], ylab="")
    xx <- c(dens$x,rev(dens$x))
    yy <- c(rep(min(dens$y), length(dens$y)), rev(dens$y))
    shade <- .getShade(color, opacity)
    polygon(xx, yy, density = NA, col = shade)
  }
}
