#**********************************************************************************
# iscam-gui-figures-timeseries.r
# This file contains the code for plotting time series values iscam outputs using the
# infrastructure provided with iscam-gui.
#
# Author            : Chris Grandin
# Development Date  : October 2013 - Present
#**********************************************************************************

plotTS <- function(plotNum  = 1,
                   multiple = FALSE,
                   retros   = FALSE,
                   endyrvec = NULL, # for use with retrospectives
                   cohorts  = NULL, # for use with squid plots (cohort retrospectives)
                   fileText = "Default",
                   btarg    = 0.4,  # Biomass target line for depletion plots
                   blim     = 0.25, # Biomass limit line for depletion plots
                   units    = .UNITS,
                   png      = .PNG,
                   silent   = .SILENT){

  # Not Applicable for iScam yet: If multiple=FALSE, SSplotTImeseries will be used to plot the single model
  # Not Applicable for iScam yet:If multiple=TRUE, SScomparison will be used to plot all models in the current sensitivity group
  # If retros=TRUE, SScomparison will be used to plot all retrospective models in the current scenario
  # Assumes that 'op' list exists and has been populated correctly.
  # Assumes that 'si' list exists and has been populated correctly.

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
  out          <- op[[scenario]]$outputs$mpd
  outSummary   <- op[[scenario]]$outputs$mpdSummary
  figDir       <- op[[scenario]]$names$figDir
  color        <- op[[scenario]]$inputs$color
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
    if(png){
      graphics.off()
      png(filename,res=res,width=width,height=height,units=units)
    }else{
      windows(width=widthScreen,height=heightScreen)
    }
    if(retros){
    }else{
    }
  }else{
    if(plotNum < 1 || plotNum > 15){
      return(FALSE)
    }
    filenameRaw  <- paste0(scenarioName,"_",fileText,".png")
    filename     <- file.path(figDir,filenameRaw)
    if(png){
      graphics.off()
      png(filename,res=res,width=width,height=height,units=units)
    }else{
      windows(width=widthScreen,height=heightScreen)
    }
    if(plotNum == 1){
      plotBiomassMPD(out, scenarioName=scenarioName, verbose = !silent, legendLoc = legendLoc, col = color)
    }
    if(plotNum == 3){
      plotDepletionMPD(out, scenarioName=scenarioName, verbose = !silent, legendLoc = legendLoc, col = color)
    }
    if(plotNum == 5){
      plotRecruitmentMPD(out, scenarioName=scenarioName, verbose = !silent, legendLoc = legendLoc, col = color)
    }

  }
  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
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
  #points(out$yr[1]-0.8, out$sbo, col=col, pch=1)
  legend(legendLoc, legend=scenarioName, col=col, lty=1, lwd=2)
	par(oldpar)
}

plotBiomassMPD <- function(out,
                           scenarioName,
                           verbose     = FALSE,
                           legendLoc   = "topright",
                           col         = 1){
	# Biomass plot for an MPD
  # 'scenarioName' is only used in the legend
  # col is the color to use in the plot
	oldpar <- par(no.readonly=T)
  bt     <- out$sbt
  yUpper <- max(bt)
  plot(out$yrs, bt, type="l", col=col,lty=1, lwd=2,ylim=c(0,yUpper),ylab="Biomass", xlab="Year", main="Biomass", las=1)
  points(out$yr[1]-0.8, out$sbo, col=col, pch=1)
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
