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

  # If multiple==FALSE then plotNum must be one of:
  # 1 Spawning biomass total (with or without uncertainty)
  # 2 Spawning biomass by area
  # 3 Spawning depletion total (with or without uncertainty)
  # 4 Spawning depletion by area
  # 5 Recruitment total (with or without uncertainty)
  # 6 Recruitment by area
  # 7 Fraction of recruitment by area

  # TODO: All multiples
  # If multiple==TRUE or retros==TRUE then plotNum must be one of:
  # 1  Spawning biomass
  # 2  Spawning biomass with uncertainty
  # 3  Biomass ratio
  # 4  Biomass ratio with uncertainty
  # 5  SPR ratio
  # 6  SPR ratio with uncertainty
  # 7  Recruitment
  # 8  Recruitment with uncertainty
  # 9  Recruitment deviations
  # 10 Recruitment deviations
  # 11 Index fits
  # 12 Index fits on log scale
  # 13 Densities - TODO: needs to be tabled so they don't overwrite each other on same device.
  # 99 Squid plot for recruitment retrospectives

  currFuncName <- getCurrFunc()
  val          <- getWinVal()
  scenario     <- val$entryScenario
  sensGroup    <- val$entrySensitivityGroup
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
    hasMCMC <- vector("numeric", length = length(models))
    for(model in 1:length(models)){
      hasMCMC[[model]] <- !is.null(op[[models[model]]]$outputs$mcmc)
    }
    out <- colors <- names <- vector("list", len <- sum(hasMCMC))
    nonmodels <- models[hasMCMC == 0]
    models <- models[hasMCMC == 1]
    if(length(nonmodels) > 0){
      for(model in 1:length(nonmodels)){
        cat0(.PROJECT_NAME,"->",currFuncName,"Model name ",op[[nonmodels[model]]]$names$scenario," has not been run in MCMC mode and is not plotted.")
      }
    }
    for(model in 1:len){
      out[[model]]    <- op[[models[model]]]$outputs$mcmc
      colors[[model]] <- op[[models[model]]]$inputs$color
      names[[model]]  <- op[[models[model]]]$names$scenario
    }
  }else{
    out <- colors <- names <- vector("list", len <- length(models))
    for(model in 1:len){
      out[[model]] <- op[[models[model]]]$outputs$mpd
      colors[[model]] <- op[[models[model]]]$inputs$color
      names[[model]] <- op[[models[model]]]$names$scenario
    }
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
    ## if(png){
    ##   graphics.off()
    ##   png(filename,res=res,width=width,height=height,units=units)
    ## }else{
    ##   windows(width=widthScreen,height=heightScreen)
    ## }
    ## if(plotNum == 1){
    ##   #plotBiomassMPD(out, scenarioName=scenarioName, verbose = !silent, legendLoc = legendLoc, col = color)
    ## }
    ## if(retros){
    ## }else{
    ## }
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
  ## if(plotNum == 3){
  ##   plotDepletionMPD(out, scenarioName=scenarioName, verbose = !silent, legendLoc = legendLoc, col = color)
  ## }
  ## if(plotNum == 5){
  ##   plotRecruitmentMPD(out, scenarioName=scenarioName, verbose = !silent, legendLoc = legendLoc, col = color)
  ## }

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
  yUpper <- max(out[[1]]$sbt, out[[1]]$sbo)
  for(model in 1:length(out)){
    yUpper <- max(yUpper, out[[model]]$sbt, out[[model]]$sbo)
  }
  plot(out[[1]]$yrs, out[[1]]$sbt, type="l", col=colors[[1]], lty=1, lwd=2,ylim=c(0,yUpper),ylab="Biomass", xlab="Year", main="Biomass", las=1)
  points(out[[1]]$yr[1]-0.8, out[[1]]$sbo, col=colors[[1]], pch=1)
  for(line in 2:length(out)){
    lines(out[[line]]$yrs, out[[line]]$sbt, type="l", col=colors[[line]], lty=1, lwd=2, ylim=c(0,yUpper))
    points(out[[line]]$yr[1]-0.8, out[[line]]$sbo, col=colors[[line]], pch=1)
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
    quants[[model]] <- getQuants(out[[model]]$sbt[[1]], ci)
  }
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]])
  }

  yrs <- as.numeric(names(out[[1]]$sbt[[1]]))

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

plotDepletionMPD <- function(out,
                             scenarioName,
                             verbose     = FALSE,
                             legendLoc   = "topright",
                             col         = 1){
	# Depletion plot for an MPD
  # 'scenarioName' is only used in the legend
  # col is the color to use in the plot
	oldpar <- par(no.readonly=T)
  bt     <- out$sbt / out$sbo
  yUpper <- max(bt)
  plot(out$yrs, bt, type="l", col=col,lty=1, lwd=2,ylim=c(0,yUpper),ylab="Depletion", xlab="Year", main="Depletion", las=1)
  legend(legendLoc, legend=scenarioName, col=col, lty=1, lwd=2)
	par(oldpar)
}

plotRecruitmentMPD <- function(out,
                               scenarioName,
                               verbose = FALSE,
                               legendLoc = "topright",
                               col = 1){
  # Recruitment plot for an MPD
  # 'scenarioName' is only used in the legend
  # col is the color to use in the plot
	oldpar	<- par(no.readonly=T)
  sage    <- out$sage
  nyear   <- length(out$yr)
	ryr     <- out$yr[(1+sage):nyear]
  plot(ryr, out$rt, lty=1, col = col, type="o", pch=19,ylim=c(0,1.2*max(out$rt)), xlab="Year",ylab="Recruits", las=1, main="Recruits")
  legend(legendLoc, legend=scenarioName, col=col, lty=1, lwd=2)
	#abline(h=median(out$rt),col=2,lty=2)
	#abline(h=mean(out$rt),col=3,lty=2)
  par(oldpar)
}

getQuants <- function(data=NULL, ci=NULL){
  # Return the column quantiles for data matrix.
  # The median along with the confidence interval 'ci'
  # will be calculated and the quantiles returned.
  currFuncName <- getCurrFunc()
  if(is.null(data)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an input posterior matrix (data).")
    return(NULL)
  }
  if(is.null(ci)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an input confidence interval in % (ci).")
    return(NULL)
  }
  ciprop <- ci / 100
  probs <- c((1-ciprop)/2,0.5,1-((1-ciprop)/2))
  quants <- apply(data, 2, quantile, probs)
  return(quants)
}

drawEnvelope <- function(yrs, quants, color, yUpper, first, ...){
  # Draw a time series envelope on a device on which plot.new has already been called
  # Assumptions: quants is a 3-row matrix,
  #  where the middle row is the median and the other two are the lower and upper
  #  values for some confidence interval.
  # yUpper is the upper limit for the y-axis
  # first is a boolean, if TRUE, plot will be called. If FALSE, lines will be called.
  lower  <- quants[1,]
  median <- quants[2,]
  upper  <- quants[3,]

  if(first){
    plot(yrs, median, type="l", col=color, lty=1, lwd=2, ylim=c(0,yUpper), ...)
  }else{
    lines(yrs, median, type="l", col=color, lty=1, lwd=2, ylim=c(0,yUpper), ...)
  }

  shade <- .getShade(color, 30)
  polyYears <- c(yrs, rev(yrs))
  polyCI    <- c(lower, rev(upper))
  polygon(polyYears, polyCI, col = shade)
}
