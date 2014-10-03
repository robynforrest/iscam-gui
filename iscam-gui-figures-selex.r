#**********************************************************************************
# ss-explore-figures-selex.r
# This file contains the code for plotting selectivity values SS outputs using the
# infrastructure provided with ss-explore.
#
# Author            : Chris Grandin
# Development Date  : October 2013
# Current version   : 1.0
#**********************************************************************************

plotSelex <- function(scenario   = 1,            # Scenario number
                      plotNum    = 1,            # Plot code number
                      savefig    = .SAVEFIG,     # TRUE/FALSE for PNG image output
                      fileText   = "Default",    # Name of the file if png==TRUE
                      plotMCMC   = FALSE,        # TRUE/FALSE to plot MCMC output
                      ci         = NULL,         # confidence interval in % (0-100)
                      multiple   = FALSE,        # TRUE/FALSE to plot sensitivity cases
                      sensGroup  = 1,            # Sensitivity group to plot if multiple==TRUE
                      index      = 1,            # Gear index to plot
                      # PlotSpecs: Width, height, and resolution of screen and file
                      ps         = list(pngres = .RESOLUTION,
                                        pngw   = .WIDTH,
                                        pngh   = .HEIGHT,
                                        res    = .RESOLUTION,
                                        w      = .WIDTH,
                                        h      = .HEIGHT),
                      leg        = "topright",   # Legend location. If NULL, none will be drawn
                      figtype    = .FIGURE_TYPE, # The filetype of the figure with period, e.g. ".png"
                      units      = .UNITS,       # Units to use in plotting
                      silent     = .SILENT
                      ){

  # plotNum must be one of:
  # 1  Logistic selectivity - age or length based will be detected automatically

  currFuncName <- getCurrFunc()

  if(plotNum != 1){
    return(FALSE)
  }
  scenarioName <- op[[scenario]]$names$scenario

  if(multiple){
    # Extract models in the current sensitivity group
    if(is.null(sens[[sensGroup]])){
      cat0(.PROJECT_NAME,"->",currFuncName,"The sensitivity group you selected has no members.")
      return(NULL)
    }
    models <- sens[[sensGroup]]
  }else{
    models <- scenario # For the non-multiple and retro cases
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
  parout <- validModels[[6]]
  controlinputs <- validModels[[7]]

  if(is.null(validModels)){
    if(is.null(names)){
      cat0(.PROJECT_NAME,"->",currFuncName,"The model ",scenarioName," has no ",type," output associated with it.\n")
    }else{
      cat0(.PROJECT_NAME,"->",currFuncName,"The model ",names[[1]]," has no ",type," output associated with it.\n")
    }
    return(NULL)
  }

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
  }else{
    filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,figtype)
    filename     <- file.path(figDir,filenameRaw)
  }

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

  if(plotNum==1){
    plotLogisticSel(scenario, out, colors, names, lty = linetypes, inputs = inputs,
                    controlinputs = controlinputs, index = index, verbose = !silent, leg = leg)
  }
  if(plotNum>=2)  cat("No Plot Yet -- Coming Soon!!\n")

  if(savefig){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

plotLogisticSel	<-	function(scenario, out, colors, names, lty, inputs, controlinputs, index, verbose, leg){
  # Currently only implemented for seltypes 1,6 and 11 (estimated logistic age-based, fixed logistic age-based, or estimated logistic length-based)
  # Both sexes will be plotted with linetype of the females = linetype for males + 1 The colors will be the same.

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  currFuncName <- getCurrFunc()
  gearNames <- inputs[[1]]$gearNames
 	ngear     <- inputs[[1]]$ngear
  nsex      <- inputs[[1]]$nsex
  aflag     <- 0 #flag to set age or length
  selType   <- controlinputs[[1]]$sel[1,index]
  selBlocks <- controlinputs[[1]]$sel[10,index] #selectivity time blocks

  if(selType != 1 && selType != 6 && selType != 11){
    cat0(.PROJECT_NAME,"->",currFuncName,"The selectivity plotting function can only plot logistic selectivity for age or length (types 1,6,11 only).")
    return(NULL)
  }
  if(inputs[[1]]$hasAgeGearNames){
    gearTitle <- gearNames[index]
  }else{
    gearTitle <- paste0("Gear ",gearNames[index])
  }

  if(selType == 1 || selType == 6){
    aflag <-1
  }else if(selType == 11){
    aflag <- 2
  }

  #startBlocks <- op[[scenario]]$inputs$control$syrtimeblock[index,]
  ## if(selBlocks > 1){
  ##   for(i in 2:selBlocks){
  ##     logselBlockData <- xx[which(logselData[,3]==startBlocks[i]),]
  ##     selData <- cbind(selData, exp(logselBlockData))
  ##   }
  ## }

  logselData <- out[[1]]$mpd$log_sel
  logselData <- logselData[which(logselData[,1]==index),]
  xx <- logselData[,4:ncol(logselData)]
  selData <- exp(xx)
  selData <- selData[1,] #selectivity in first block
  selData <- as.matrix(selData)
  if(aflag == 1){
    # logistic age selectivity is not sex-specific
    Xlab <- "Age"
    age <- out[[1]]$mpd$age
    plot(age, selData[,1], type="l", xlab=Xlab, ylab="Proportion", lwd=2, lty=lty[[1]], col=colors[[1]], las=1, main=gearTitle, ylim=c(0,1.1))
    ## if(selBlocks > 1){
    ##   # TODO: implement this. currently only works right for a single selectivity block
    ##   for(i in 2:selBlocks){
    ##     lines(age, selData[,i], lty=i, col=i, lwd=2)
    ##     legtext <- c(legtext, paste("Selectivity Block",i,":", startBlocks[i]))
    ##   }
    ## }
    if(length(out) > 1){
      for(model in 2:length(out)){
        age        <- out[[model]]$mpd$age
        logselData <- out[[model]]$mpd$log_sel
        logselData <- logselData[which(logselData[,1]==index),]
        xx         <- logselData[,4:ncol(logselData)]
        selData    <- exp(xx)
        selData    <- selData[1,] #selectivity in first block
        selData    <- as.matrix(selData)
        lines(age, selData[,1], type="l", lwd=2, lty=lty[[model]], col=colors[[model]], las=1, main=gearTitle, ylim=c(0,1.1))
      }
    }
  }else if(aflag == 2){
    Xlab <- "Length (cm)"
    if(nsex == 2){
      len        <- out[[1]]$mpd$la
      plot(len[1,], selData[,1], type="l", xlab=Xlab, ylab="Proportion", lwd=2, col=colors[[1]], lty=lty[[1]], las=1,  main=gearTitle, ylim=c(0,1.1))
      lines(len[2,], selData[,1], type="l", lwd=2, lty=lty[[1]]+1, col=colors[[1]], las=1)
      currName <- names[[1]]
      names[[1]] <- paste0(currName, " - Male")
      female <- paste0(currName, " - Female")
      names <- append(names, female)
      colors <- append(colors, colors[[1]])
      lty <- append(lty, lty[[1]]+1)
      ## if(selBlocks > 1){
      ##   for(i in 2:selBlocks){
      ##     lines(len[1,], selData[,i], lty=i, col=i, lwd=2)
      ##     lines(len[2,], selData[,i], lty=i+1, col=i, lwd=2)
      ##     legtext <- c(legtext, paste("Male - Selectivity Block",i,":", startBlocks[i]))
      ##     legtext <- c(legtext, paste("Female - Selectivity Block",i,":", startBlocks[i]))
      ##   }
      ## }
      if(length(out) > 1){
        for(model in 2:length(out)){
          len        <- out[[model]]$mpd$la
          logselData <- out[[model]]$mpd$log_sel
          logselData <- logselData[which(logselData[,1]==index),]
          xx         <- logselData[,4:ncol(logselData)]
          selData    <- exp(xx)
          selData    <- selData[1,] #selectivity in first block
          selData    <- as.matrix(selData)
          lines(len[1,], selData[,1], type="l", lwd=2, lty=lty[[model]], col=colors[[model]], las=1, main=gearTitle, ylim=c(0,1.1))
          lines(len[2,], selData[,1], type="l", lwd=2, lty=lty[[model]]+1, col=colors[[model]], las=1, main=gearTitle, ylim=c(0,1.1))
          currName <- names[[model]]
          names[[model]] <- paste0(currName, " - Male")
          female <- paste0(currName, " - Female")
          names <- append(names, female)
          colors <- append(colors, colors[[model]])
          lty <- append(lty, lty[[model]]+1)
        }
      }
    }else{
      len     <- out[[model]]$mpd$la
      plot(len, selData[,1], type="l", xlab=Xlab, ylab="Proportion", lwd=2, col=1, las=1,  main=gearTitle, ylim=c(0,1.1))
      legtext <- c(legtext, paste("Combined sexes"))
      if(selBlocks>1){
        for(i in 2:selBlocks){
          lines(len, selData[,i], lty=i, col=i, lwd=2)
          legtext <- c(legtext, paste("Selectivity Block",i,":", startBlocks[i]))
        }
      }
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }
}
