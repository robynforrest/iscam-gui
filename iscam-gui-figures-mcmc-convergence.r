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

  oldPar <- par()
  currFuncName <- getCurrFunc()
  val          <- getWinVal()
  burnin       <- val$burn
  thinning     <- val$thin
  scenario     <- val$entryScenario
  scenarioName <- op[[scenario]]$names$scenario
  sensGroup    <- val$entrySensitivityGroup
  mcmcOut      <- op[[scenario]]$outputs$mcmc
  if(is.null(mcmcOut)){
    cat0(.PROJECT_NAME,"->",currFuncName,"The model ",scenarioName," has no mcmc output data associated with it.\n")
    return(NULL)
  }
  mcmcData <- mcmcOut$params
  numParams <- ncol(mcmcData)
  # The next line is a simple algorithm, just generate the smallest square grid
  # that will hold all of the parameter trace plots.
  nrows <- ncols <- ceiling(sqrt(numParams))
	par(mfrow=c(nrows, ncols), las=1)
  mcmcData <- window(as.matrix(mcmcData), start=burnin, thin=thinning)
  for(param in 1:numParams){
    par(mar=c(2,4,2,2))
    mcmcTrace <- as.matrix(mcmcData[,param])
    plot(mcmcTrace,main=colnames(mcmcData)[param],type="l",ylab="",xlab="",axes=F)
    box()
    at <- labels <- seq(0,nrow(mcmcData), 200)
    axis(1, at=at, labels=labels)
    axis(2)
  }
  par(oldPar)
}
