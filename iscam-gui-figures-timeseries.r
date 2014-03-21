#**********************************************************************************
# ss-explore-figures-timeseries.r
# This file contains the code for plotting time series values SS outputs using the
# infrastructure provided with ss-explore.
#
# Author            : Chris Grandin
# Development Date  : October 2013 - 2014
# Current version   : 1.0
#**********************************************************************************

plotTS <- function(plotNum  = 1,
                   val      = NULL, # window val argument as you would get from PBSModelling's getWinVal()
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

  # If multiple=FALSE, SSplotTImeseries will be used to plot the single model
  # If multiple=TRUE, SScomparison will be used to plot all models in the current sensitivity group
  # If retros=TRUE, SScomparison will be used to plot all retrospective models in the current scenario
  # Assumes that 'op' list exists and has been populated correctly.
  # Assumes that 'si' list exists and has been populated correctly.

  # If multiple==FALSE then plotNum must be one of:
  # 1  Total biomass total all areas
  # 2  Total biomass by area
  # 3  Total biomass in all areas in spawning season
  # 4  Summary biomass total all areas
  # 5  Summary biomass by area
  # 6  Summary biomass in all areas in spawning season
  # 7  Spawning biomass total (with or without uncertainty)
  # 8  Spawning biomass by area
  # 9  Spawning depletion total (with or without uncertainty)
  # 10 Spawning depletion by area
  # 11 Recruitment total (with or without uncertainty)
  # 12 Recruitment by area
  # 13 Fraction of recruitment by area
  # 14 Recruitment by birth season
  # 15 Fraction of recruitment by birth season

  # If multiple==TRUE or retros==TRUEthen plotNum must be one of:
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
  out          <- op[[scenario]]$outputs$mpd
  outSummary   <- op[[scenario]]$outputs$mpdSummary
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
    if(png){
      graphics.off()
      png(filename,res=res,width=width,height=height,units=units)
    }else{
      windows(width=widthScreen,height=heightScreen)
    }
    if(retros){
      plotDat <- op[[scenario]]$outputs$retrosSummary
      mcmcCheck <- FALSE
      modelNames <- NULL
      modelNames[[1]] <- op[[scenario]]$names$scenario
      for(retro in 1:length(op[[scenario]]$outputs$retros)){
        #if(op[[scenario]]$outputs$retros[[retro]]$isMCMC){
        #  mcmcCheck <- TRUE
        #}
        modelNames[[retro+1]] <- op[[scenario]]$outputs$retros[[retro]]$names$scenario
      }

      if(mcmcCheck){
        cat(.PROJECT_NAME,"->",currFuncName,"Warning - One or more Retrospectives for scenario ",op[[scenarios]]$names$scenario,
            " is an MCMC run, but SSplotComparison only works with MLE runs.\n",sep="")
        return(FALSE)
      }
    }else{
      plotDat <- sens[[sensGroup]]$summary
      mcmcCheck <- sens[[sensGroup]]$isMCMC
      modelNames <- sens[[sensGroup]]$names
      if(mcmcCheck){
        cat(.PROJECT_NAME,"->",currFuncName,"Warning - Sensitivity group ",sensGroup,
            " contains a Scenario which is an MCMC run, but SSplotComparison only works with MLE runs.\n",sep="")
        return(FALSE)
      }

    }
    if(plotNum==99){
      SSplotRetroRecruits(retroSummary = plotDat,
                          relative     = FALSE,
                          endyrvec     = endyrvec,
                          cohorts      = cohorts,
                          ylim         = c(-3,3),
                          legendloc    = legendLoc)
    }else if(retros){
      SSplotComparisons(summaryoutput = plotDat,
                        endyrvec      = endyrvec,
                        subplots      = c(plotNum),
                        legendlabels  = modelNames,
                        minbthresh    = blim,
                        btarg         = btarg,
                        verbose       = !silent,
                        legendloc     = legendLoc)
    }else{
      SSplotComparisons(summaryoutput = plotDat,
                        subplots      = c(plotNum),
                        legendlabels  = modelNames,
                        minbthresh    = blim,
                        btarg         = btarg,
                        verbose       = !silent,
                        legendloc     = legendLoc)
    }

  }else{
    if(plotNum < 1 || plotNum > 15){
      return(FALSE)
    }
    filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,".png")
    filename     <- file.path(figDir,filenameRaw)
    if(png){
      graphics.off()
      png(filename,res=res,width=width,height=height,units=units)
    }
    SSplotTimeseries(out,
                     plot     = TRUE,
                     print    = FALSE,
                     subplot  = plotNum,
                     pheight  = height,
                     pwidth   = width,
                     punits   = units,
                     res      = res,
                     verbose  = !silent,
                     legendLoc= legendLoc)
  }
  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}
