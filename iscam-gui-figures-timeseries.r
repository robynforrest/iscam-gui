#**********************************************************************************
# iscam-gui-figures-timeseries.r
# This file contains the code for plotting time series values iscam outputs using the
# infrastructure provided with iscam-gui.
#
# Author            : Chris Grandin
# Development Date  : October 2013 - Present
#**********************************************************************************

plotTS <- function(scenario   = 1,         # Scenario number
                   plotNum    = 1,         # Plot code number
                   savefig    = .SAVEFIG,  # TRUE/FALSE for plot output
                   fileText   = "Default", # Name of the file if png==TRUE
                   plotMCMC   = FALSE,     # TRUE/FALSE to plot MCMC output
                   ci         = NULL,      # confidence interval in % (0-100)
                   multiple   = FALSE,     # TRUE/FALSE to plot sensitivity cases
                   retros     = FALSE,     # TRUE/FALSE to plot retropectives
                   sensGroup  = 1,         # Sensitivity group to plot if multiple==TRUE
                   index      = 1,         # Survey index to plot if plotNum==7
                   burnthin   = list(0,1), # List of two elements, burnin and thinning for mcmc plots
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
                   recrOffset = 0.1,          # Recruitment bar offset used if multiple==TRUE
                   btarg      = 0.4,          # Biomass target line for depletion plots
                   blim       = 0.25,         # Biomass limit line for depletion plots
                   units      = .UNITS,       # Units to use in plotting
                   silent     = .SILENT){

  # If multiple==TRUE, whatever is in the sensitivity list (sens) for the currently
  #  chosen sensitivity number in the GUI will be plotted.
  # If multiple==FALSE, whatever the currently chosen scenario number is in the GUI
  #  will be plotted by itself.
  # If retros==TRUE, whatever retrospectives which are loaded on the current scenario
  #  will be plotted against the base. 'multiples' and 'plotMCMC' will be ignored if retros==TRUE.
  # If plotMCMC==TRUE, follow the same rules, but for the MCMC data. Use the
  #  confidence interval for an envelope plot in this case.
  # Assumes that 'op' list exists and has been populated correctly.
  # Assumes that 'sens' list exists and has been populated correctly.
  # plotSpecs is a list of length 6 holding values:
  #  pngres (png resolution), pngw (png width), pngh (png height),
  #  res (on-screen resolution), w (on-screen width),h (on-screen height)

  # If plotNum must be one of:
  # 1 Spawning biomass total
  # 2 Spawning biomass by area
  # 3 Spawning depletion total
  # 4 Spawning depletion by area
  # 5 Recruitment total
  # 6 Recruitment by area
  # 7 Index fit
  # 8 SPR ratio
  # 9 Fishing mortality
  #10 Reference Points
  #11 Recruitment deviations

  currFuncName <- getCurrFunc()

  #retroDat     <- op[[scenario]]$outputs$retros
  scenarioName <- op[[scenario]]$names$scenario
  if(retros){
    multiple <- FALSE
    plotMCMC <- FALSE
  }
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
  }else if(!retros){
    # This is the single plot case, no multiples and no retros
    type <- "mpd"
    validModels <- getValidModelsList(models, type = type)
  }else{
    type <- "mpd"
    validModels <- getValidModelsList(models, retros = TRUE, type = type)
  }

  out    <- validModels[[1]]
  colors <- validModels[[2]]
  names  <- validModels[[3]]
  inputs <- validModels[[4]]
  linetypes <- validModels[[5]]
  parout <- validModels[[6]]

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

  if(plotNum < 1 || plotNum > 11){
    cat0(.PROJECT_NAME,"->",currFuncName,"The plotNum must be between 1 and 10. You passed ",plotNum)
    return(FALSE)
  }
  if(multiple || retros){
    if(retros){
      filenameRaw  <- paste0("Retrospective_",op[[scenario]]$names$scenario,"_",fileText,figtype)
      filename     <- file.path(op[[scenario]]$names$dir,.FIGURES_DIR_NAME,filenameRaw)
    }else{
      filenameRaw  <- paste0("SensitivityGroup_",sensGroup,"_",fileText,figtype)
      filename     <- file.path(.SENS_FIGURES_DIR_NAME,filenameRaw)
    }
  }else{
    filenameRaw  <- paste0(scenarioName,"_",fileText,figtype)
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

  if(plotNum == 1){
    if(plotMCMC){
      plotBiomassMCMC(out, colors, names, burnthin = burnthin, ci, verbose = !silent, leg = leg, showtitle = showtitle)
    }else{
      plotBiomassMPD(out, colors, names, lty = linetypes, verbose = !silent, leg = leg, showtitle = showtitle)
    }
  }
  if(plotNum == 3){
    if(plotMCMC){
      plotDepletionMCMC(out, colors, names, burnthin = burnthin, ci, verbose = !silent, leg = leg, showtitle = showtitle)
    }else{
      plotDepletionMPD(out, colors, names, lty = linetypes, verbose = !silent, leg = leg, showtitle = showtitle)
    }
  }
  if(plotNum == 5){
    if(plotMCMC){
      plotRecruitmentMCMC(out, colors, names, ci, burnthin = burnthin, offset=recrOffset, verbose = !silent, leg = leg, showtitle = showtitle)
    }else{
      plotRecruitmentMPD(out, colors, names, lty = linetypes, verbose = !silent, leg = leg, showtitle = showtitle)
    }
  }
  if(plotNum == 7){
    if(plotMCMC){
      cat0(.PROJECT_NAME,"->",currFuncName,"MCMC plots for Indices not implemented.")
    }else{
      plotIndexMPD(scenario, out, inputs, index, colors, names, linetypes, verbose = !silent, leg = leg, showtitle = showtitle)
    }
  }
  if(plotNum == 8){
    if(plotMCMC){
      #plotSPRMCMC(out, colors, names, inputs, ci, index = index, verbose = !silent, leg = leg)
    }else{
      #plotSPRMPD(out, colors, names, inputs, index = index, verbose = !silent, leg = leg)
    }
  }
  if(plotNum == 9){
    if(plotMCMC){
      plotFMCMC(out, colors, names, ci, burnthin = burnthin, verbose = !silent, leg = leg, showtitle = showtitle)
      #cat0(.PROJECT_NAME,"->",currFuncName,"MCMC plots for F not implemented.")
    }else{
      plotFMPD(out, colors, names, verbose = !silent, leg = leg, showtitle = showtitle)
    }
  }
  if(plotNum == 10){
    if(plotMCMC){
      plotReferencePointsMCMC(out, colors, names, ci, burnthin = burnthin, verbose = !silent, showtitle = showtitle)
    }else{
      cat0(.PROJECT_NAME,"->",currFuncName,"Cannot make MPD plots for reference points, run MCMC first.")
    }
  }
  if(plotNum == 11){
    if(plotMCMC){
      plotRecruitmentDevsMCMC(out, colors, names, ci, burnthin = burnthin, offset=recrOffset, verbose = !silent, leg = leg, showtitle = showtitle)
    }else{
      plotRecruitmentDevsMPD(out, parout, colors, names, lty = linetypes, verbose = !silent, leg = leg, showtitle = showtitle)
    }
  }

  if(savefig){
    cat0(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename)
    dev.off()
  }
  return(TRUE)
}

plotBiomassMPD <- function(out       = NULL,
                           colors    = NULL,
                           names     = NULL,
                           lty       = NULL,
                           verbose   = FALSE,
                           showtitle = TRUE,
                           leg = "topright"){
  # Biomass plot for an MPD
  # out is a list of the mpd outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  currFuncName <- getCurrFunc()

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

  yUpper <- max(out[[1]]$mpd$sbt, out[[1]]$mpd$sbo)
   if(out[[1]]$mpd$sbo > 2*max(out[[1]]$mpd$sbt)){
    # When sbo is very large, the trends in sbt are masked - don't plot sbt if more than twice the max value of sbt
    yUpper <- 1.1*max(out[[1]]$mpd$sbt)
  }
  for(model in 1:length(out)){
    if(out[[model]]$mpd$sbo > 2*max(yUpper, out[[model]]$mpd$sbt)){
      yUpper <- max(yUpper, 1.1*out[[model]]$mpd$sbt)
   }else{
     yUpper <- max(yUpper, out[[model]]$mpd$sbt, out[[model]]$mpd$sbo)
   }
  }
  par(mar=c(3,6,3,3))
  title <- ""
  if(showtitle){
    title <- "Spawning Biomass"
  }
  plot(out[[1]]$mpd$yrs, out[[1]]$mpd$sbt, type="l", col=colors[[1]], lty=lty[[1]], lwd=2,ylim=c(0,yUpper),ylab="Biomass (1000 mt)\n", xlab="Year", main=title, las=1)
  points(out[[1]]$mpd$yr[1], out[[1]]$mpd$sbo, col=colors[[1]], pch=20)
  if(length(out) > 1){
    for(line in 2:length(out)){
      lines(out[[line]]$mpd$yrs, out[[line]]$mpd$sbt, type="l", col=colors[[line]], lty=lty[[line]], lwd=2, ylim=c(0,yUpper))
      points(out[[line]]$mpd$yr[1], out[[line]]$mpd$sbo, col=colors[[line]], pch=20)
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }
}

plotBiomassMCMC <- function(out       = NULL,
                            colors    = NULL,
                            names     = NULL,
                            ci        = NULL,
                            burnthin  = list(0,1),
                            verbose   = FALSE,
                            showtitle = TRUE,
                            leg = "topright"){
  # Biomass plot for an MCMC
  # out is a list of the mcmc outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # ci is the confidence interval to use in percent, eg. 95

  currFuncName <- getCurrFunc()
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
  if(is.null(names)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a confidence interval in % (ci).")
    return(NULL)
  }
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
    sbt <- window(mcmc(out[[model]]$mcmc$sbt[[1]]), start=burn, thin=thin)
    quants[[model]] <- getQuants(sbt, ci)
  }
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]])
  }

  yrs <- as.numeric(names(out[[1]]$mcmc$sbt[[1]]))
  par(mar=c(3,6,3,3))
  title <- ""
  if(showtitle){
    title <- "Spawning Biomass"
  }
  drawEnvelope(yrs, quants[[1]], colors[[1]], yUpper, first=TRUE, ylab="Biomass (1000 mt)\n", xlab="Year", main=title, las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], yUpper, first=FALSE)
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
}

plotDepletionMPD <- function(out       = NULL,
                             colors    = NULL,
                             names     = NULL,
                             lty       = NULL,
                             verbose   = FALSE,
                             showtitle = TRUE,
                             leg = "topright"){
  # Depletion plot for an MPD
  # out is a list of the mpd outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  currFuncName <- getCurrFunc()
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

  depl <- out[[1]]$mpd$sbt / out[[1]]$mpd$sbo
  yUpper <- max(depl)
  for(model in 1:length(out)){
    depl <- out[[model]]$mpd$sbt / out[[model]]$mpd$sbo
    yUpper <- max(yUpper, depl)
  }
  depl <- out[[1]]$mpd$sbt / out[[1]]$mpd$sbo
  title <- ""
  if(showtitle){
    title <- "Reletive Spawning Biomass"
  }
  plot(out[[1]]$mpd$yrs, depl, type="l", col=colors[[1]], lty=lty[[1]], lwd=2,ylim=c(0,yUpper),ylab="Depletion", xlab="Year", main=title, las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      depl <- out[[line]]$mpd$sbt / out[[line]]$mpd$sbo
      lines(out[[line]]$mpd$yrs, depl, type="l", col=colors[[line]], lty=lty[[line]], lwd=2, ylim=c(0,yUpper))
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }
}

plotDepletionMCMC <- function(out       = NULL,
                              colors    = NULL,
                              names     = NULL,
                              ci        = NULL,
                              burnthin  = list(0,1),
                              verbose   = FALSE,
                              showtitle = TRUE,
                              leg = "topright"){
  # Depletion plot for an MCMC
  # out is a list of the mcmc outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # TODO: These lists should be modified by the code so that only
  #  MCMC models will be included on the plot and legend.
  currFuncName <- getCurrFunc()
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
  if(is.null(names)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a confidence interval in % (ci).")
    return(NULL)
  }
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
    sbt <- window(mcmc(out[[model]]$mcmc$sbt[[1]]), start=burn, thin=thin)
    bo <- as.vector(window(mcmc(out[[model]]$mcmc$params$bo), start=burn, thin=thin))
    depl <- sbt / bo
    quants[[model]] <- getQuants(depl, ci)
  }
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]])
  }

  yrs <- as.numeric(names(out[[1]]$mcmc$sbt[[1]]))
  title <- ""
  if(showtitle){
    title <- "Reletive Spawning Biomass"
  }
  drawEnvelope(yrs, quants[[1]], colors[[1]], yUpper, first=TRUE, ylab="Depletion", xlab="Year", main=title, las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], yUpper, first=FALSE)
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
}

plotRecruitmentMPD <- function(out       = NULL,
                               colors    = NULL,
                               names     = NULL,
                               lty       = NULL,
                               verbose   = FALSE,
                               showtitle = TRUE,
                               leg = "topright"){
  # Recruitment plot for an MPD
  # out is a list of the mpd outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  currFuncName <- getCurrFunc()
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

  sage   <- out[[1]]$mpd$sage
  nyear  <- length(out[[1]]$mpd$yr)
  ryr    <- out[[1]]$mpd$yr[(1+sage):nyear]
  rt     <- out[[1]]$mpd$rt

  yUpper <- max(rt)
  for(model in 1:length(out)){
    tmprt  <- out[[model]]$mpd$rt
    yUpper <- max(yUpper, tmprt)
  }
  xlim <- c(min(ryr), max(ryr))
  if(length(out)>1){
    for(model in 1:length(out)){
      tmpsage  <- out[[model]]$mpd$sage
      tmpnyear <- length(out[[model]]$mpd$yr)
      tmpryr   <- out[[model]]$mpd$ryr[(1+tmpsage):nyear]
      minx     <- min(min(tmpryr), min(xlim))
      maxx     <- max(max(tmpryr), max(xlim))
      xlim     <- c(minx, maxx)
    }
  }
  title <- ""
  if(showtitle){
    title <- "Recruitment"
  }

  plot(ryr, rt, type = "o", col=colors[[1]], pch=19, lty=lty[[1]], lwd=2, ylim=c(0,yUpper), xlim=xlim,
       ylab="Recruitment (millions)", xlab="Year", main=title, las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      sage <- out[[line]]$mpd$sage
      nyear <- length(out[[line]]$mpd$yr)
      ryr   <- out[[line]]$mpd$yr[(1+sage):nyear]
      rt    <- out[[line]]$mpd$rt
      lines(ryr, rt, type="o",col=colors[[line]], pch=19, lty=lty[[line]], lwd=2, ylim=c(0,yUpper), las=1)
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }
}

plotRecruitmentMCMC <- function(out       = NULL,
                                colors    = NULL,
                                names     = NULL,
                                ci        = NULL,
                                burnthin  = list(0,1),
                                offset    = 0.1,
                                verbose   = FALSE,
                                showtitle = TRUE,
                                leg = "topright"){
  # Recruitment plot for an MCMC
  # out is a list of the mcmc outputs to show on the plot
  # colors is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # ci is the confidence interval in percent, eg 95
  # offset is the number of years to offset the points and bars
  #  for clarity on the plot, i.e. so that there is no overlapping.
  # TODO: These lists should be modified by the code so that only
  #  MCMC models will be included on the plot and legend.
  currFuncName <- getCurrFunc()
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
  if(is.null(names)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a confidence interval in % (ci).")
    return(NULL)
  }
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
   #quants[[model]] <- getQuants(out[[model]]$mcmc$rt[[1]], ci)
   rt <- window(mcmc(out[[model]]$mcmc$rt[[1]]), start=burn, thin=thin)
   quants[[model]] <- getQuants(rt, ci)
  }
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]])
  }

  yrs <- as.numeric(names(out[[1]]$mcmc$rt[[1]]))

  #RF - need to get xlim in case time series lengths differ
  Xlim <- c(min(yrs), max(yrs))
  if(length(out)>1){
    for(model in 1:length(out)){
      tmpryr     <- as.numeric(names(out[[model]]$mcmc$rt[[1]]))
      minx <- min(min(tmpryr), min(Xlim))
      maxx <- max(max(tmpryr), max(Xlim))
      Xlim <- c(minx, maxx)
    }
  }
  title <- ""
  if(showtitle){
    title <- "Recruitment"
  }

  plot(yrs, quants[[1]][2,], type="p", pch=20, col=colors[[1]], ylim=c(0,yUpper), xlim=Xlim, xlab="Year", ylab="Recruitment (millions)", main=title, las=1)
  arrows(yrs, quants[[1]][1,],
         yrs, quants[[1]][3,], col=colors[[1]], code=3, angle=90, length=0.01)
  if(length(out) > 1){
    incOffset <- offset
    for(line in 2:length(out)){
      yrs <- as.numeric(names(out[[line]]$mcmc$rt[[1]]))
      # Plot the uncertainty
      points(yrs+incOffset, quants[[line]][2,], pch=20, col=colors[[line]])
      arrows(yrs+incOffset, quants[[line]][1,],
             yrs+incOffset, quants[[line]][3,], col=colors[[line]], code=3, angle=90, length=0.01)
      incOffset <- incOffset + offset
    }
  }
  #RF added median and mean for 1 scenario plot
  if(length(out) == 1){
    abline(h=median(as.matrix(rt)),col=2, lty=1)
    abline(h=mean(as.matrix(rt)),col=3,lty=1)
    if(!is.null(leg))  legend(leg, legend=c(names,"Long-term median","MCMC long-term mean"), col=c(unlist(colors),2,3), lty=1, lwd=2)
  }
  if(length(out) > 1){
    if(!is.null(leg))  legend(leg, legend=names, col=unlist(colors), lty=1, lwd=2)
  }

}

plotRecruitmentDevsMPD <- function(out       = NULL,
                                   parout    = NULL,
                                   colors    = NULL,
                                   names     = NULL,
                                   lty       = NULL,
                                   verbose   = FALSE,
                                   showtitle = TRUE,
                                   leg = "topright"){
  # Recruitment deviations plot for an MPD
  # out is a list of the mpd outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  currFuncName <- getCurrFunc()
  if(is.null(out)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an output vector (out).")
    return(NULL)
  }
  if(is.null(parout)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a parameter output vector (parout).")
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

  ryr    <- out[[1]]$mpd$yr
  rt     <- parout[[1]]$log_rec_devs

  # Get y-limits
  maxy <- max(rt)
  miny <- min(rt)
  for(model in 1:length(out)){
    tmprt  <- parout[[model]]$log_rec_devs
    miny <- min(miny, min(tmprt))
    maxy <- max(maxy, tmprt)
    ylim <- c(miny, maxy)
  }
  # Get x-limits
  xlim <- c(min(ryr), max(ryr))
  if(length(out)>1){
    for(model in 1:length(out)){
      tmpryr   <- out[[model]]$mpd$yr
      minx     <- min(min(tmpryr), min(xlim))
      maxx     <- max(max(tmpryr), max(xlim))
      xlim     <- c(minx, maxx)
    }
  }
  title <- ""
  if(showtitle){
    title <- "Recruitment deviations"
  }

  plot(ryr, rt, type = "o", col=colors[[1]], pch=19, lty=lty[[1]], lwd=2, ylim=ylim, xlim=xlim,
       ylab="Recruitment deviations (millions)", xlab="Year", main=title, las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      ryr   <- out[[line]]$mpd$yr
      rt    <- parout[[line]]$log_rec_devs
      lines(ryr, rt, type="o",col=colors[[line]], pch=19, lty=lty[[line]], lwd=2, ylim=ylim, las=1)
    }
  }
  abline(0, 0, lty=1, lwd=1, col="lightgreen")
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }
}

plotIndexMPD <- function(scenario  = NULL,
                         out       = NULL,
                         inputs    = NULL,
                         index     = NULL,
                         colors    = NULL,
                         names     = NULL,
                         lty       = NULL,
                         verbose   = FALSE,
                         showtitle = TRUE,
                         leg = "topright"){
  # Index fits plot for an MPD
  # scenario is the sccenario number. Only used if 'out' is of length 1.
  # out is a list of the mpd outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # Notes:
  # - Models may have different gears than others, but we want the index plots to match by gear.
  #   The solution is to match them by name if plotting multiple (sensitivity plots)
  #   by creating a unique vector of names which is the union of all names across all models
  #   and using that to match to the names in each model, only plotting if the name is found.

  currFuncName <- getCurrFunc()
  if(is.null(scenario)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a scenario number.")
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
  if(is.null(lty)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a linetypes vector (lty).")
    return(NULL)
  }
  if(is.null(names)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a names vector (names).")
    return(NULL)
  }
  if(is.null(inputs)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an inputs list (inputs).")
    return(NULL)
  }
  if(is.null(index)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an index number for plotting (index).")
    return(NULL)
  }
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  # Get a list of unique index names across all models to be included in this plot
  inpvec <- unlist(inputs)
  indexnames <- unique(inpvec[grep("indexGearNames",names(inpvec))])
  if(is.null(indexnames)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply index names in the data files to plot indices across models.")
    return(NULL)
  }
  indexnames <- unique(indexnames)
  if(index > length(indexnames)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an index number less or equal to ",length(indexnames)," (index).")
    return(NULL)
  }
  currindexname <- indexnames[index]
  # The 'indexnames' vector will be used as the index to scroll through,
  # i.e. when the user changes to the next index, the next name in this list will
  # be matched.
  gearTitle <- indexnames[index]
  mat <- NULL

  for(model in 1:length(out)){
    gearnum <- match(currindexname, inputs[[model]]$indexGearNames)
    if(is.na(gearnum)){
      # Remove the gear from the legend lists, using the property that if a list
      # element is set to NULL, it will be removed completely from the list.
      lty[[model]] <- NA
      colors[[model]] <- NA
      names[[model]] <- NA
    }else{
      inputindices <- as.data.frame(inputs[[model]]$indices[[gearnum]])
      outputit     <- as.data.frame(out[[model]]$mpd$it_hat)
      if(ncol(outputit) == 1){
        dat          <- as.numeric(outputit[,1])
      }else{
        dat          <- as.numeric(outputit[gearnum,])
      }
      tmpindices   <- as.data.frame(inputs[[model]]$indices[[gearnum]])
      yrs          <- tmpindices$iyr
      cv           <- 1 / inputindices$wt
      dat          <- dat[!is.na(dat)]
      mat          <- cbind(mat, dat)
    }
  }
  title <- ""
  if(showtitle){
    title <- paste0("Index fit - ",gearTitle)
  }
  # Change the NAs to NULLs for the legend variables, using the property that if a list
  # element is set to NULL, it will be removed completely from the list.
  lty[sapply(lty, is.na)] <- NULL
  colors[sapply(colors, is.na)] <- NULL
  names[sapply(names, is.na)] <- NULL
  matplot(yrs, mat, type = "l", lwd = 2, lty = unlist(lty), col = unlist(colors),
          las = 1, main = title, ylim = c(0,max(mat, inputindices$it + cv * inputindices$it)), xlab="Year", ylab="x 1000 metric tonnes")
  points(yrs, inputindices$it, pch = 3)
  arrows(yrs, inputindices$it + cv * inputindices$it ,yrs, inputindices$it - cv * inputindices$it,
         code = 3, angle = 90, length = 0.01, col = "black")

  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }
}

plotFMPD <- function(out       = NULL,
                     colors    = NULL,
                     names     = NULL,
                     pch       = 20,
                     pointSize = 0.2,
                     verbose   = FALSE,
                     showtitle = TRUE,
                     leg = "topright"){
  # Fishing mortality plot for an MPD
  # out is a list of the mpd outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # pch and pointsize are passed to the plot function. pointSize is in fact 'cex'
  currFuncName <- getCurrFunc()
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
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  nsex   <- out[[1]]$mpd$nsex
  yrs    <- out[[1]]$mpd$yr
  nyear  <- length(yrs)
  ngear  <- out[[1]]$mpd$ngear

  # This is the F with the # of rows being the number of gears, and that is multiplied
  # by the number of sexes. So for a 3-gear, 2 sex model, there will be 6 rows in f with main grouping
  # by gear, i.e. two, three-row groupings.
  numModels <- length(out)
  f         <- out[[1]]$mpd$ft
  yUpper    <- max(f)
  for(model in 1:length(out)){
    yUpper <- max(yUpper, out[[model]]$mpd$ft)
  }
  legendNames <- NULL
  legendLines <- NULL
  legendCols  <- NULL

  for(sex in 1:nsex){
    for(gear in 1:ngear){
      meanF <- f[((sex-1) * ngear + gear),]
      if(numModels == 1){
        color <- gear
      }else{
        color <- colors[[1]]
      }
      if(all(meanF == 0)){
        #cat0(.PROJECT_NAME,"->",currFuncName,"All meanFs for scenario ",names[[1]],", gear ",gear,", and sex ",sex," are 0 so it is not plotted.")
      }else{
        if(sex == 1 && gear == 1){
          # First one, so use plot command
          title <- ""
          if(showtitle){
            title <- "Fishing Mortality"
          }
          plot(yrs, meanF, type = "o", col=color, pch=pch, cex=pointSize, lty=sex, lwd=2, ylim=c(0,yUpper), ylab="Mean F", xlab="Year", main=title, las=1)
        }else{
          lines(yrs, meanF, type = "o", col=color, pch=pch, cex=pointSize, lty=sex, lwd=2)
        }
        if(sex == 1){
          legendNames <- c(legendNames, paste0(names[[1]]," gear ",gear," - Female"))
        }else{
          legendNames <- c(legendNames, paste0(names[[1]]," gear ",gear," - Male"))
        }
        legendLines <- c(legendLines, (sex-1) * ngear + gear)
        legendCols  <- c(legendCols, color)
      }
    }
  }

  # THIS IS FOR F-at-age outputs to be done later perhaps
  # If sex==2 then every even row of F will be females and every odd row will be males
  ## if(nsex == 2){
  ##   # Split the matrix into odd and even matrices
  ##   rowNums  <- seq(1,nrow(f))
  ##   oddRows  <- !!(rowNums %% 2)
  ##   evenRows <- !(rowNums %% 2)
  ##   fMale    <- f[oddRows,]
  ##   fFemale  <- f[evenRows,]
  ##   plot(yrs, fMale, type = "b", col=colors[[1]], pch=19, lty=1, lwd=2,ylim=c(0,yUpper),ylab="F", xlab="Year", main="Fishing Mortality", las=1)
  ##   points(yrs, fFemale, type = "b", col=colors[[1]], pch=19, lty=1, lwd=2)
  ## }else{
  ##   plot(yrs, f, type = "b", col=colors[[1]], pch=19, lty=1, lwd=2,ylim=c(0,yUpper),ylab="F", xlab="Year", main="Fishing Mortality", las=1)
  ## }

  if(length(out) > 1){
    for(line in 2:length(out)){  # For each model in the sensitivity list..
      nsex   <- out[[line]]$mpd$nsex
      yrs    <- out[[line]]$mpd$yr
      nyear  <- length(yrs)
      ngear  <- out[[line]]$mpd$ngear
      f      <- out[[line]]$mpd$ft
      for(sex in 1:nsex){
        for(gear in 1:ngear){
          meanF <- f[((sex-1) * ngear + gear),]
          if(all(meanF == 0)){
            #cat0(.PROJECT_NAME,"->",currFuncName,"All meanFs for scenario ",names[[line]],", gear ",gear,", and sex ",sex," are 0 so it is not plotted.")
          }else{
            lines(yrs, meanF, type = "o", col=colors[[line]], pch=pch, cex=pointSize, lty=sex, lwd=2)
            if(sex == 1){
              legendNames <- c(legendNames, paste0(names[[line]]," gear ",gear," - Female"))
            }else{
              legendNames <- c(legendNames, paste0(names[[line]]," gear ",gear," - Male"))
            }
            legendLines <- c(legendLines, (sex-1) * ngear + gear)
            legendCols  <- c(legendCols, colors[[line]])
          }
        }
      }
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=legendNames, col=legendCols, lty=legendLines, lwd=2)
  }
}

plotFMCMC <- function(out       = NULL,
                      colors    = NULL,
                      names     = NULL,
                      ci        = NULL,
                      burnthin  = list(0,1),
                      pch       = 20,
                      pointSize = 0.2,
                      verbose   = FALSE,
                      showtitle = TRUE,
                      leg = "topright"){
  # Fishing mortality plot for mcmc models
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # ci is the confidence interval to use in percent, eg. 95
  # pch and pointsize are passed to the plot function. pointSize is in fact 'cex'

  currFuncName <- getCurrFunc()
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
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # Calculate quantiles for the posterior data
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
    # the following has a f[[1]]. The correct way would be to have iscam only output gear 1 instead of all gears,
    # similar to sbt output.
    ft <- window(mcmc(out[[model]]$mcmc$ft[[1]][[1]]), start=burn, thin=thin)
    quants[[model]] <- getQuants(ft, ci)
  }
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]])
  }
  # Get last four digits of the names
  ynames <- names(out[[1]]$mcmc$ft[[1]][[1]])
  pattern <- ".*_([0-9]+)"
  yrs <- as.numeric(sub(pattern,"\\1",ynames))
  par(mar=c(3,6,3,3))
  title <- ""
  if(showtitle){
    title <- "Fishing Mortality"
  }
  drawEnvelope(yrs, quants[[1]], colors[[1]], yUpper, first=TRUE, ylab="F\n", xlab="Year", main=title, las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], yUpper, first=FALSE)
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=1, lwd=2)
  }

}


plotReferencePointsMCMC <- function(out       = NULL,
                                    colors    = NULL,
                                    names     = NULL,
                                    ci        = NULL,
                                    burnthin  = list(0,1),
                                    pch       = 20,
                                    pointSize = 0.2,
                                    verbose   = FALSE,
                                    showtitle = TRUE,
                                    leg = "topright"){
  # Reference points plot for an MCMC model
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # ci is the confidence interval to use in percent, eg. 95
  # pch and pointsize are passed to the plot function. pointSize is in fact 'cex'
  currFuncName <- getCurrFunc()
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
  if(is.null(names)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a confidence interval in % (ci).")
    return(NULL)
  }

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  bo   <- as.vector(window(mcmc(out[[1]]$mcmc$params$bo), start=burn, thin=thin))
  bmsy <- as.vector(window(mcmc(out[[1]]$mcmc$params$bmsy), start=burn, thin=thin))
  msy  <- as.vector(window(mcmc(out[[1]]$mcmc$params$msy1), start=burn, thin=thin))
  fmsy <- as.vector(window(mcmc(out[[1]]$mcmc$params$fmsy1), start=burn, thin=thin))

  if(length(out) > 1){
    for(model in 2:length(out)){
      bo   <- cbind(bo, as.vector(window(mcmc(out[[model]]$mcmc$params$bo), start=burn, thin=thin)))
      bmsy <- cbind(bmsy, as.vector(window(mcmc(out[[model]]$mcmc$params$bmsy), start=burn, thin=thin)))
      msy  <- cbind(msy, as.vector(window(mcmc(out[[model]]$mcmc$params$msy1), start=burn, thin=thin)))
      fmsy <- cbind(fmsy, as.vector(window(mcmc(out[[model]]$mcmc$params$fmsy1), start=burn, thin=thin)))
    }
  }

  colors <- c(do.call("cbind",colors)) # Convert colors list to vector
  names  <- c(do.call("cbind",names))

  par(mfrow=c(2,2), mai=c(0.3,0.5,0.4,0.2), oma=c(1.,1.2,0.2,0.1))
  ymax <- max(fmsy)
  boxplot(fmsy, pch=pch, range = ci/100, names=names, border=colors, main="FMSY", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,ymax))
  ymax <- max(msy)
  boxplot(msy, pch=pch, range = ci/100, names=names, border=colors, main="MSY (1000mt)", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,ymax))
  ymax <- max(bo)
  boxplot(bo, pch=pch, range = ci/100, names=names, border=colors, main="B0 (1000mt)", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,ymax))
  ymax <- max(bmsy)
  boxplot(bmsy, pch=pch, range = ci/100, names=names, border=colors, main="BMSY (1000mt)", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,ymax))
}
