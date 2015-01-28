#**********************************************************************************
# iscam-gui-figures-mle-diagnostics.r
# This file contains the code to plot MLE diagnostics information
# such as Objective function value, max gradient, # of function evaluations,
# hang codes, and exit codes.
#
# Author            : Chris Grandin
# Development Date  : January 2015 - Present
#**********************************************************************************

plotDiagnostics <- function(scenario   = 1,         # Scenario number
                            plotNum    = 1,         # Plot code number
                            savefig    = .SAVEFIG,  # TRUE/FALSE for plot output
                            fileText   = "Default", # Name of the file if png==TRUE
                            # PlotSpecs: Width, height, and resolution of screen and file
                            ps         = list(pngres = .RESOLUTION,
                                              pngw   = .WIDTH,
                                              pngh   = .HEIGHT,
                                              res    = .RESOLUTION,
                                              w      = .WIDTH,
                                              h      = .HEIGHT),
                            figtype    = .FIGURE_TYPE, # The filetype of the figure with period, e.g. ".png"
                            burnthin   = list(0,1), # List of two elements, burnin and thinning
                            exFactor        = 1.5,
                            units           = .UNITS,
                            silent          = .SILENT){

  # Plot diagnostics for an MLE run
  # plotNum must be one of:
  # 1 Objective Function Value
  # 2 Maximum gradient
  # 3 Function evaluations
  # 4 Hang codes
  # 5 Exit codes

  currFuncName <- getCurrFunc()
  scenarioName <- op[[scenario]]$names$scenario
  #mcmcOut      <- op[[scenario]]$outputs$mcmc
  mpdData      <- op[[scenario]]$outputs$mpd
  if(is.null(mpdData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"The model ",scenarioName," has no MPD output data associated with it.\n")
    return(NULL)
  }

  figDir       <- op[[scenario]]$names$figDir
  res          <- ps$pngres
  width        <- ps$pngw
  height       <- ps$pngh
  resScreen    <- ps$res
  widthScreen  <- ps$w
  heightScreen <- ps$h

  filenameRaw  <- paste0(scenarioName,"_",fileText,figtype)
  filename     <- file.path(figDir,filenameRaw)
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
#browser()
  if(plotNum == 1){
    plotObjFunVal(mpdData)
  }
  if(plotNum == 2){
    plotMaxGradient(mpdData)
  }
  if(plotNum == 3){
    plotFuncEvals(mpdData)
  }
  if(plotNum == 4){
    plotHangCodes(mpdData)
  }
  if(plotNum == 5){
    plotExitCodes(mpdData)
  }
  if(savefig){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

plotObjFunVal <- function(dat = NULL){
  # Objective function values for runs

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))
browser()
  # At this point, dat$ObjectiveFunction, dat$MaxGrad, dat$ExitCode, and dat$HangCode are present.
  # Need to merge them by comparison group
  
  currFuncName <- getCurrFunc()
  if(is.null(dat)){
    cat0(.PROJECT_NAME,"->",currFuncName,"dat must be supplied.\n")
    return(NULL)
  }
}

# FROM THE CCAM DAYS..
plotRuntimeStats <- function(type=0,ylab=""){
  # plots runtime stats for all scenarios.
  # assumes all scenarios have the same number of projected years
  # assumes the opList structure is used.
  # types:
  # 1 = objFun, 2 = max gradient, 3 = number of function evals, 4 = hangcode, any other value = exit code
  try(dev.off(),silent=T)
  if(type==1 | type==2 | type==3){ # use PLOTBUBBLES from PBSModelling for these ones
    if(type==1){
      # Objective function values
      # GREEN means value is positive GOOD
      # RED means a bad objective function value, i.e. returned -1,#IND (which is represented as 0.0 from GrMPE)
      dat <- abs(opList[[1]][[4]]$ObjectiveFunction)
      for(scenario in 2:length(opList)){
        if(opList[[scenario]][[6]]){ # if MPD results are loaded
          dat <- rbind(dat,opList[[scenario]][[4]]$ObjectiveFunction)
        }else{
          dat <- rbind(dat,NA)
          cat(paste("Error plotting 'Runtime Statistics' figure.  There are no MPD outputs loaded for scenario",scenario,", it is NULL on the plot.\n\n"))
        }
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
        if(opList[[scenario]][[6]]){ # if MPD results are loaded
          dat <- rbind(dat,opList[[scenario]][[4]]$MaxGrad)
        }else{
          dat <- rbind(dat,NA)
          cat(paste("Error plotting 'Runtime Statistics' figure.  There are no MPD outputs loaded for scenario",scenario,", it is NULL on the plot.\n\n"))
        }
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
      for(scenario in 2:length(opList)){
        if(opList[[scenario]][[6]]){ # if MPD results are loaded
          dat <- rbind(dat,opList[[scenario]][[4]]$nf)
        }else{
          dat <- rbind(dat,NA)
          cat(paste("Error plotting 'Runtime Statistics' figure.  There are no MPD outputs loaded for scenario",scenario,", it is NULL on the plot.\n\n"))
        }
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
    for(scenario in 2:length(opList)){
      if(type==4){
        plotcharCol <- ifelse(opList[[scenario]][[4]]$HangCode==1,"red","green")
        title("Hang code values")
      }else{
        plotcharCol <- ifelse(opList[[scenario]][[4]]$ExitCode==1,"green","red")
        plotcharCol <- ifelse(opList[[scenario]][[4]]$ExitCode==2,"blue",plotcharCol)
        plotcharCol <- ifelse(opList[[scenario]][[4]]$ExitCode==3,"purple",plotcharCol)
        title("Exit code values")
      }
      points(scenario,1,pch=.PCHCODE,col=plotcharCol)
    }
  }
}
