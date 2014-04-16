#**********************************************************************************
# iscam-gui-figures-timeseries.r
# This file contains the code for plotting time series values iscam outputs using the
# infrastructure provided with iscam-gui.
#
# Author            : Chris Grandin
# Development Date  : October 2013 - Present
#**********************************************************************************

plotTS <- function(plotNum  = 1,
                   png      = .PNG,
                   fileText = "Default",
                   plotMCMC = FALSE,
                   ci       = NULL, # confidence interval in %
                   multiple = FALSE,
                   retros   = FALSE,
                   btarg    = 0.4,  # Biomass target line for depletion plots
                   blim     = 0.25, # Biomass limit line for depletion plots
                   units    = .UNITS,
                   silent   = .SILENT){

  # If multiple = TRUE, whatever is in the sensitivity list (sens) for the currently
  #  chosen sensitivity number in the GUI will be plotted.
  # If multiple = FALSE, whatever the currently chosen scenario number is in the GUI
  #  will be plotted by itself.
  # If plotMCMC = TRUE, follow the same rules, but for the MCMC data. Use the
  #  confidence interval for an envelope plot in this case.
  # If retros=TRUE, plot all retrospective models in the currently chosen scenario
  #  number in the GUI.
  # Assumes that 'op' list exists and has been populated correctly.
  # Assumes that 'sens' list exists and has been populated correctly.

  # If plotNum must be one of:
  # 1 Spawning biomass total (with or without uncertainty)
  # 2 Spawning biomass by area
  # 3 Spawning depletion total (with or without uncertainty)
  # 4 Spawning depletion by area
  # 5 Recruitment total (with or without uncertainty)
  # 6 Recruitment by area
  # 7 Index fits

  currFuncName <- getCurrFunc()
  val          <- getWinVal()
  scenario     <- val$entryScenario
  sensGroup    <- val$entrySensitivityGroup
  index        <- val$entryIndex

  #retros       <- op[[scenario]]$outputs$retros
  scenarioName <- op[[scenario]]$names$scenario
  if(multiple){
    # Extract models in the current sensitivity group
    if(is.null(sens[[sensGroup]])){
      cat0(.PROJECT_NAME,"->",currFuncName,"The sensitivity group you selected has no members.")
      return(NULL)
    }
    models <- sens[[sensGroup]]
  }else{
    models <- scenario # For the non-multiple case
  }

  if(plotMCMC){
    # Remove models which do not have MCMC outputs
    type <- "mcmc"
    validModels <- getValidModelsList(models, type)
  }else{
    type <- "mpd"
    validModels <- getValidModelsList(models, type)
  }
  out    <- validModels[[1]]
  colors <- validModels[[2]]
  names  <- validModels[[3]]
  inputs <- validModels[[4]]
  if(is.null(validModels)){
    cat0(.PROJECT_NAME,"->",currFuncName,"The model ",names[[1]]," has no ",type," output associated with it.\n")
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
  if(multiple || retros){
    if(plotNum < 1 || plotNum > 13 && plotNum != 99){
      return(FALSE)
    }
    if(retros){
      filenameRaw  <- paste0("Retrospective_",op[[scenario]]$names$scenario,"_",fileText,".png")
      filename     <- file.path(op[[scenario]]$names$dir,.FIGURES_DIR_NAME,filenameRaw)
    }else{
      filenameRaw  <- paste0("SensitivityGroup_",sensGroup,"_",fileText,".png")
      filename     <- file.path(.SENS_FIGURES_DIR_NAME,filenameRaw)
    }
  }else{
    if(plotNum < 1 || plotNum > 15){
      return(FALSE)
    }
    filenameRaw  <- paste0(scenarioName,"_",fileText,".png")
    filename     <- file.path(figDir,filenameRaw)
  }
  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)
  }else{
    windows(width=widthScreen,height=heightScreen)
  }
  if(plotNum == 1){
    if(plotMCMC){
      plotBiomassMCMC(out, colors, names, ci, verbose = !silent, legendLoc = legendLoc)
    }else{
      plotBiomassMPD(out, colors, names, verbose = !silent, legendLoc = legendLoc)
    }
  }
  if(plotNum == 3){
    if(plotMCMC){
      plotDepletionMCMC(out, colors, names, ci, verbose = !silent, legendLoc = legendLoc)
    }else{
      plotDepletionMPD(out, colors, names, verbose = !silent, legendLoc = legendLoc)
    }
  }
  if(plotNum == 5){
    if(plotMCMC){
      plotRecruitmentMCMC(out, colors, names, ci, verbose = !silent, legendLoc = legendLoc)

    }else{
      plotRecruitmentMPD(out, colors, names, verbose = !silent, legendLoc = legendLoc)
    }
  }
  if(plotNum == 7){
    if(plotMCMC){
      plotIndexMCMC(out, colors, names, inputs, ci, index = index, verbose = !silent, legendLoc = legendLoc)

    }else{
      plotIndexMPD(out, colors, names, inputs, index = index, verbose = !silent, legendLoc = legendLoc)
    }
  }

  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

plotBiomassMPD <- function(out       = NULL,
                           colors    = NULL,
                           names     = NULL,
                           verbose   = FALSE,
                           legendLoc = "topright"){
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
	oldpar <- par(no.readonly=T)
  yUpper <- max(out[[1]]$mpd$sbt, out[[1]]$mpd$sbo)
  for(model in 1:length(out)){
    yUpper <- max(yUpper, out[[model]]$mpd$sbt, out[[model]]$mpd$sbo)
  }
  plot(out[[1]]$mpd$yrs, out[[1]]$mpd$sbt, type="l", col=colors[[1]], lty=1, lwd=2,ylim=c(0,yUpper),ylab="Biomass", xlab="Year", main="Biomass", las=1)
  points(out[[1]]$mpd$yr[1]-0.8, out[[1]]$mpd$sbo, col=colors[[1]], pch=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      lines(out[[line]]$mpd$yrs, out[[line]]$mpd$sbt, type="l", col=colors[[line]], lty=1, lwd=2, ylim=c(0,yUpper))
      points(out[[line]]$mpd$yr[1]-0.8, out[[line]]$mpd$sbo, col=colors[[line]], pch=1)
    }
  }
  if(!is.null(legendLoc)){
    legend(legendLoc, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
	par(oldpar)
}

plotBiomassMCMC <- function(out       = NULL,
                            colors    = NULL,
                            names     = NULL,
                            ci        = NULL,
                            verbose   = FALSE,
                            legendLoc = "topright"){
	# Biomass plot for an MCMC
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
	oldpar <- par(no.readonly=T)
  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
    quants[[model]] <- getQuants(out[[model]]$mcmc$sbt[[1]], ci)
  }
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]])
  }

  yrs <- as.numeric(names(out[[1]]$mcmc$sbt[[1]]))

  drawEnvelope(yrs, quants[[1]], colors[[1]], yUpper, first=TRUE, ylab="Biomass", xlab="Year", main="Biomass", las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], yUpper, first=FALSE)
    }
  }
  if(!is.null(legendLoc)){
    legend(legendLoc, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
  par(oldpar)
}

plotDepletionMPD <- function(out       = NULL,
                             colors    = NULL,
                             names     = NULL,
                             verbose   = FALSE,
                             legendLoc = "topright"){
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
	oldpar <- par(no.readonly=T)
  depl <- out[[1]]$mpd$sbt / out[[1]]$mpd$sbo
  yUpper <- max(depl)
  for(model in 1:length(out)){
    depl <- out[[model]]$mpd$sbt / out[[model]]$mpd$sbo
    yUpper <- max(yUpper, depl)
  }
  depl <- out[[1]]$mpd$sbt / out[[1]]$mpd$sbo
  plot(out[[1]]$mpd$yrs, depl, type="l", col=colors[[1]], lty=1, lwd=2,ylim=c(0,yUpper),ylab="Depletion", xlab="Year", main="Depletion", las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      depl <- out[[line]]$mpd$sbt / out[[line]]$mpd$sbo
      lines(out[[line]]$mpd$yrs, depl, type="l", col=colors[[line]], lty=1, lwd=2, ylim=c(0,yUpper))
    }
  }
  if(!is.null(legendLoc)){
    legend(legendLoc, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
	par(oldpar)
}

plotDepletionMCMC <- function(out       = NULL,
                              colors    = NULL,
                              names     = NULL,
                              ci        = NULL,
                              verbose   = FALSE,
                              legendLoc = "topright"){
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
	oldpar <- par(no.readonly=T)
  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
    depl <- out[[model]]$mcmc$sbt[[1]] / out[[model]]$mcmc$params$bo
    quants[[model]] <- getQuants(depl, ci)
  }
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]])
  }

  yrs <- as.numeric(names(out[[1]]$mcmc$sbt[[1]]))

  drawEnvelope(yrs, quants[[1]], colors[[1]], yUpper, first=TRUE, ylab="Depletion", xlab="Year", main="Depletion", las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], yUpper, first=FALSE)
    }
  }
  if(!is.null(legendLoc)){
    legend(legendLoc, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
  par(oldpar)
}

plotRecruitmentMPD <- function(out       = NULL,
                               colors    = NULL,
                               names     = NULL,
                               verbose   = FALSE,
                               legendLoc = "topright"){
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
	oldpar <- par(no.readonly=T)
  sage   <- out[[1]]$mpd$sage
  nyear  <- length(out[[1]]$mpd$yr)
  ryr    <- out[[1]]$mpd$yr[(1+sage):nyear]
  rt     <- out[[1]]$mpd$rt

  yUpper <- max(rt)
  for(model in 1:length(out)){
    rt     <- out[[1]]$mpd$rt
    yUpper <- max(yUpper, rt)
  }
  plot(ryr, rt, type="l", col=colors[[1]], lty=1, lwd=2,ylim=c(0,yUpper),ylab="Recruitment", xlab="Year", main="Recruitment", las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      nyear <- length(out[[line]]$mpd$yr)
      ryr   <- out[[line]]$mpd$yr[(1+sage):nyear]
      rt    <- out[[line]]$mpd$rt
      lines(ryr, rt, type="l", col=colors[[line]], lty=1, lwd=2, ylim=c(0,yUpper))
    }
  }
  if(!is.null(legendLoc)){
    legend(legendLoc, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
	par(oldpar)
}

plotRecruitmentMCMC <- function(out       = NULL,
                                colors    = NULL,
                                names     = NULL,
                                ci        = NULL,
                                verbose   = FALSE,
                                legendLoc = "topright"){
	# Recruitment plot for an MCMC
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
	oldpar <- par(no.readonly=T)
  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
    rt <- out[[1]]$mcmc$rt[[1]]
    quants[[model]] <- getQuants(rt, ci)
  }
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]])
  }

  yrs <- as.numeric(names(out[[1]]$mcmc$rt[[1]]))

  drawEnvelope(yrs, quants[[1]], colors[[1]], yUpper, first=TRUE, ylab="Recruitment", xlab="Year", main="Recruitment", las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], yUpper, first=FALSE)
    }
  }
  if(!is.null(legendLoc)){
    legend(legendLoc, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
  par(oldpar)
}

plotIndexMPD <- function(out       = NULL,
                         colors    = NULL,
                         names     = NULL,
                         inputs    = NULL,
                         index     = NULL,
                         verbose   = FALSE,
                         legendLoc = "topright"){
	# Index fits plot for an MPD
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
  if(is.null(inputs)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an inputs list (inputs).")
    return(NULL)
  }
  if(is.null(index)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an index number for plotting (index).")
    return(NULL)
  }
  if(index > length(inputs[[1]]$indices)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an index number less or equal to ",length(inputs[[1]]$indices)," (index).")
    return(NULL)
  }
	oldpar <- par(no.readonly=T)

  # Get the plotting limits by looking through the input lists and outputs of indices
  inputindices <- inputs[[1]]$indices[[index]]
  yUpper <- max(inputindices[,2])  # it column (index value) - NOTE 2 is hardwired (it). If this function breaks look here!
  minYear <- min(inputindices[,1]) # yr column - NOTE 1 is hardwired (it). If this function breaks look here!
  maxYear <- max(inputindices[,1]) # yr column - NOTE 1 is hardwired (it). If this function breaks look here!
  for(model in 1:length(out)){
    inputindices <- inputs[[model]]$indices[[index]]
    outputit <- out[[model]]$mpd$it_hat[index,]
    inputit  <- inputindices[,2] # NOTE 2 is hardwired (it). If this function breaks look here!
    yUpper   <- max(yUpper, inputit, outputit, na.rm=TRUE) # NA is removed here because surveys have different years, and missing ones are NA
    minYear1 <- min(inputindices[,1]) # yr column
    maxYear1 <- max(inputindices[,1]) # yr column
    minYear  <- min(minYear,  minYear1)
    maxYear  <- max(maxYear, maxYear1)
  }
  dat <- out[[1]]$mpd$it_hat[index,]
  yrs <- inputs[[1]]$indices[[index]][,1]
  dat <- dat[!is.na(dat)]
  plot(yrs, dat, type="l", col=colors[[1]], lty=1, lwd=2, xlim=c(minYear,maxYear),ylim=c(0,yUpper),ylab="Biomass", xlab="Year", main="Index fits", las=1)
  points(yrs, inputindices[,2], pch=3)
  if(length(out) > 1){
    for(model in 2:length(out)){
      dat <- out[[model]]$mpd$it_hat[index,]
      yrs <- inputs[[model]]$indices[[index]][,1]
      dat <- dat[!is.na(dat)]
      lines(yrs, dat,  type="l", col=colors[[model]], lty=1, lwd=2)
    }
  }
  if(!is.null(legendLoc)){
    legend(legendLoc, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
	par(oldpar)
}

plotIndexMCMC <- function(out       = NULL,
                          colors    = NULL,
                          names     = NULL,
                          inputs    = NULL,
                          ci        = NULL,
                          verbose   = FALSE,
                          legendLoc = "topright"){
	# Recruitment plot for an MCMC
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
	oldpar <- par(no.readonly=T)
  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
    rt <- out[[1]]$mcmc$rt[[1]]
    quants[[model]] <- getQuants(rt, ci)
  }
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]])
  }

  yrs <- as.numeric(names(out[[1]]$mcmc$rt[[1]]))

  drawEnvelope(yrs, quants[[1]], colors[[1]], yUpper, first=TRUE, ylab="Recruitment", xlab="Year", main="Recruitment", las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], yUpper, first=FALSE)
    }
  }
  if(!is.null(legendLoc)){
    legend(legendLoc, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
  par(oldpar)
}
