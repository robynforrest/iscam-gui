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
                   recrShowFinalYear = TRUE,  # Show the final year in recruitment plots
                   btarg      = 0.4,          # Biomass target line for depletion plots
                   blim       = 0.25,         # Biomass limit line for depletion plots
                   units      = .UNITS,       # Units to use in plotting
                   silent     = .SILENT,
                   showSbio   = FALSE,        # Show Spawning biomass on Vulnerable biomass plot
                   plotU      = FALSE,        # Plot U instead of F for the fishing mortality plot
                   indfixaxis = FALSE,        # Fix the index x-axis so that all indices or plotted on the same year scale
                   indletter  = NULL,         # A letter to plot on the panel. If NULL, no letter will be printed.
                   showumsy   = FALSE,        # Plot Umsy instead of Fmsy for reference point plot
                   colors     = NULL,         # Allow a color vector to be input (for use with latex). If NULL, colors will come from gui.
                   linetypes  = NULL,         # Allow a linetypes vector to be input (for use with latex). If NULL, linetypes will come from gui.
                   names      = NULL,         # Allow a names vector to be input (for use with latex). If NULL, names will come from gui.
                   shortnames = NULL,         # Short names, mainly for use with the reference points plot, so that names will be visible
                   showB0Ref  = FALSE,        # Show the 0.2 and 0.4 B0 lines on the spawning biomass mcmc plot
                   showBMSYRef= FALSE,        # Show the 0.4 and 0.8 BMSY lines on the spawning biomass mcmc plot
                   add        = FALSE,        # If TRUE, plot will be added to current device
                   opacity    = 90            # Opaqueness (opposite of transparency) with which to draw envelopes
                   ){

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
  # 1 Spawning biomass all areas
  # 2 Spawning biomass by area
  # 3 Spawning depletion total
  # 4 Spawning depletion by area
  # 5 Recruitment total
  # 6 Recruitment by area
  # 7 Index fit
  # 8 SPR ratio
  # 9 Fishing mortality (F or U depending on the plotU argument)
  #10 Reference Points
  #11 Recruitment deviations
  #12 Vulnerable biomass all areas (+Spawning biomass all areas)
  #13 Relative spawning biomass with USR (0.8MSY) and LRP (0.4MSY) with uncertainties and 0.2B0 and 0.4B0 lines

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
  if(is.null(colors)){
    colors <- validModels[[2]]
  }
  if(is.null(names)){
    names  <- validModels[[3]]
  }
  inputs <- validModels[[4]]
  if(is.null(linetypes)){
    linetypes <- validModels[[5]]
  }
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

  if(plotNum < 1 || plotNum > 13){
    cat0(.PROJECT_NAME,"->",currFuncName,"The plotNum must be between 1 and 12. You passed ",plotNum)
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
  }else if(!add){
    windows(width=widthScreen,height=heightScreen)
  }

  if(plotNum == 1){
    if(plotMCMC){
      plotBiomassMCMC(out, colors, names, burnthin = burnthin, ci, verbose = !silent, leg = leg, showtitle = showtitle, showB0Ref = showB0Ref, showBMSYRef = showBMSYRef, opacity=opacity, add=add)
    }else{
      if(showBMSYRef){
        cat0(.PROJECT_NAME,"->",currFuncName,"BMSY reference line not available in MPD mode.")
      }
      plotBiomassMPD(out, colors, names, lty = linetypes, verbose = !silent, leg = leg, showtitle = showtitle, showB0Ref = showB0Ref, opacity=opacity)
    }
  }
  if(plotNum == 3){
    if(plotMCMC){
      plotDepletionMCMC(out, colors, names, burnthin = burnthin, ci, verbose = !silent, leg = leg, showtitle = showtitle, opacity=opacity, add=add)
    }else{
      plotDepletionMPD(out, colors, names, lty = linetypes, verbose = !silent, leg = leg, showtitle = showtitle, opacity=opacity, add=add)
    }
  }
  if(plotNum == 5){
    if(plotMCMC){
      plotRecruitmentMCMC(out, colors, names, ci, burnthin = burnthin, offset=recrOffset, verbose = !silent, leg = leg, showtitle = showtitle, add=add, recrShowFinalYear=recrShowFinalYear)
    }else{
      plotRecruitmentMPD(out, colors, names, lty = linetypes, verbose = !silent, leg = leg, showtitle = showtitle, add=add, recrShowFinalYear=recrShowFinalYear)
    }
  }
  if(plotNum == 7){
    if(plotMCMC){
      cat0(.PROJECT_NAME,"->",currFuncName,"MCMC plots for Indices not implemented. Plotting MPD.")
    }
    plotIndexMPD(scenario, out, inputs, index, colors, names, linetypes, verbose = !silent, leg = leg, showtitle = showtitle, indfixaxis=indfixaxis, add=add)
  }
  if(plotNum == 9){
    if(plotMCMC){
      plotFMCMC(out, colors, names, ci, burnthin = burnthin, verbose = !silent, leg = leg, showtitle = showtitle, plotU=plotU, opacity=opacity, add=add)
    }else{
      plotFMPD(out, colors, names, verbose = !silent, leg = leg, showtitle = showtitle, plotU=plotU, opacity=opacity, add=add)
    }
  }
  if(plotNum == 10){
    if(plotMCMC){
      plotReferencePointsMCMC(out, colors, names, ci, burnthin = burnthin, verbose = !silent, showtitle = showtitle, shortnames=shortnames, showumsy=showumsy, add=add)
    }else{
      cat0(.PROJECT_NAME,"->",currFuncName,"Cannot make MPD plots for reference points, run MCMC first.")
    }
  }
  if(plotNum == 11){
    if(plotMCMC){
      plotRecruitmentDevsMCMC(out, colors, names, ci, burnthin = burnthin, offset=recrOffset, verbose = !silent, leg = leg, showtitle = showtitle, add=add)
    }else{
      plotRecruitmentDevsMPD(out, parout, colors, names, lty = linetypes, verbose = !silent, leg = leg, showtitle = showtitle, add=add)
    }
  }
  if(plotNum == 12){
    # Vulnerable biomass
    if(plotMCMC){
      plotVBiomassMCMC(out, colors, names, burnthin = burnthin, ci, verbose = !silent, leg = leg, showtitle = showtitle, showSbio = showSbio, opacity=opacity, add=add)
    }else{
      plotVBiomassMPD(out, colors, names, lty = linetypes, verbose = !silent, leg = leg, showtitle = showtitle, showSbio = showSbio, opacity=opacity, add=add)
    }
  }

  if(plotNum == 13){
    # Relative spawning biomass with USR (0.8MSY) and LRP (0.4MSY) with uncertainties and 0.2B0 and 0.4B0 lines (for the SAR)
    if(plotMCMC){
      plotSAR(out, colors, names, burnthin = burnthin, ci, verbose = !silent, leg = leg, showtitle = showtitle, opacity=opacity, add=add)
    }else{
      cat0(.PROJECT_NAME,"->",currFuncName,"No Relative biomass/USR/LRP plot available for MPD runs. Check the plot MCMC box.")
    }
  }

  if(!is.null(indletter)){
    .gletter(indletter)
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
                           showB0Ref = TRUE,
                           leg       = "topright",
                           add       = FALSE,
                           opacity   = 90){
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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

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
  #par(mar=c(3,6,3,3))
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
  # Add 0.2B0, and 0.4B0 lines for the reference case [[1]] to plot
  if(showB0Ref){
    abline(h=0.2*out[[1]]$mpd$sbo, col="red", lty=2)
    mtext("0.2B0",2,at=0.2*out[[1]]$mpd$sbo,col="red",las=1)
    abline(h=0.4*out[[1]]$mpd$sbo, col="green", lty=2)
    mtext("0.4B0",2,at=0.4*out[[1]]$mpd$sbo,col="green",las=1)
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }
}

plotBiomassMCMC <- function(out         = NULL,
                            colors      = NULL,
                            names       = NULL,
                            ci          = NULL,
                            burnthin    = list(0,1),
                            offset      = 0.1,
                            verbose     = FALSE,
                            showtitle   = TRUE,
                            showB0Ref   = FALSE,  # Show the 0.2 and 0.4 B0 lines on the plot
                            showBMSYRef = FALSE,  # Show the 0.4 and 0.8 BMSY lines on the plot
                            leg         = "topright",
                            add         = FALSE,
                            opacity     = 90){
  # Biomass plot for an MCMC
  # out is a list of the mcmc outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # ci is the confidence interval to use in percent, eg. 95
  # offset is the number of years to offset the points and bars

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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  quants <- vector("list", length(out))
  boquants <- vector("list", length(out))
  for(model in 1:length(out)){
    sbo <- window(mcmc(out[[model]]$mcmc$params$bo), start=burn, thin=thin)
    sbt <- window(mcmc(out[[model]]$mcmc$sbt[[1]]), start=burn, thin=thin)
    quants[[model]] <- getQuants(sbt, ci)
    boquants[[model]] <- getQuants(sbo, ci)
  }
  yUpper <- max(quants[[1]], boquants[[1]][3])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]], boquants[[1]][3])
  }

  yrs <- as.numeric(names(out[[1]]$mcmc$sbt[[1]]))
  #par(mar=c(3,6,3,3))
  title <- ""
  if(showtitle){
    title <- "Spawning Biomass"
  }
  drawEnvelope(yrs, quants[[1]], colors[[1]], 0, yUpper, first=TRUE, opacity=opacity, ylab="Biomass (1000 mt)\n", xlab="Year", main=title, las=1)
  # Draw SB0 and uncertainty over top
  incOffset <- offset
  points(yrs[1] - incOffset, boquants[[1]][2], pch = 19, col = colors[[1]])
  arrows(yrs[1] - incOffset, boquants[[1]][1], yrs[1] - incOffset, boquants[[1]][3], lwd = 2, code = 0, col = colors[[1]])
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], 0, yUpper, first=FALSE, opacity=opacity)
      incOffset <- incOffset + offset
      points(yrs[1] - incOffset, boquants[[line]][2], pch = 19, col = colors[[line]])
      arrows(yrs[1] - incOffset, boquants[[line]][1], yrs[1] - incOffset, boquants[[line]][3], lwd = 2, code = 0, col = colors[[line]])
    }
  }
  # Add 0.4BMSY, 0.8BMSY, 0.2B0, and 0.4B0 lines for the reference case [[1]] to plot
  if(showB0Ref){
    abline(h=0.2*boquants[[1]][2], col="red", lty=1, lwd=2)  # sbo1 set above in loop through models
    mtext("0.2B0",2,at=0.2*boquants[[1]][2],col="red",las=1)
    abline(h=0.4*boquants[[1]][2], col="green", lty=1, lwd=2)
    mtext("0.4B0",2,at=0.4*boquants[[1]][2],col="green",las=1)
  }
  if(showBMSYRef){
    bmsy <- window(mcmc(out[[1]]$mcmc$params$bmsy), start=burn, thin=thin)
    bmsyquants <- getQuants(bmsy, ci)
    abline(h=0.4*bmsyquants[2], col="red", lty=2, lwd=2)
    mtext("0.4BMSY",2,at=0.4*bmsyquants[2],col="red",las=1)
    abline(h=0.8*bmsyquants[2], col="green", lty=2, lwd=2)
    mtext("0.8BMSY",2,at=0.8*bmsyquants[2],col="green",las=1)
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
                             leg       = "topright",
                             add       = FALSE,
                             opacity   = 90){
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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

  depl <- out[[1]]$mpd$sbt / out[[1]]$mpd$sbo
  for(model in 1:length(out)){
    depl <- out[[model]]$mpd$sbt / out[[model]]$mpd$sbo
  }
  depl <- out[[1]]$mpd$sbt / out[[1]]$mpd$sbo
  title <- ""
  if(showtitle){
    title <- "Relative Spawning Biomass"
  }
  plot(out[[1]]$mpd$yrs, depl, type="l", col=colors[[1]], lty=lty[[1]], lwd=2,ylim=c(0,1.1),ylab="Relative Spawning Biomass", xlab="Year", main=title, las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      depl <- out[[line]]$mpd$sbt / out[[line]]$mpd$sbo
      lines(out[[line]]$mpd$yrs, depl, type="l", col=colors[[line]], lty=lty[[line]], lwd=2, ylim=c(0,1))
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
                              leg       = "topright",
                              add       = FALSE,
                              opacity   = 90){
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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

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
    title <- "Relative Spawning Biomass"
  }
  drawEnvelope(yrs, quants[[1]], colors[[1]], 0, max(1.1,yUpper), first=TRUE, opacity=opacity, ylab="Relative Spawning Biomass", xlab="Year", main=title, las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], 0, yUpper, first=FALSE, opacity=opacity)
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
}

plotSAR <- function(out       = NULL,
                    colors    = NULL,
                    names     = NULL,
                    ci        = NULL,
                    burnthin  = list(0,1),
                    verbose   = FALSE,
                    showtitle = TRUE,
                    leg       = "topright",
                    add       = FALSE,
                    opacity   = 90){
  # Relative spawning biomass with USR (0.8MSY) and LRP (0.4MSY) with uncertainties and 0.2B0 and 0.4B0 lines (for the SAR)
  # out is a list of the mcmc outputs to show on the plot
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
  if(is.null(names)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a confidence interval in % (ci).")
    return(NULL)
  }
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,3.1))

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
    title <- "Relative Spawning Biomass/USR/LRP"
  }
  drawEnvelope(yrs, quants[[1]], colors[[1]], 0, max(1.1,yUpper), first=TRUE, opacity=opacity,
               #ylab=expression("Relative Spawning Biomass (" ~ frac(B[t],B[0]) ~ ")"),
               ylab=expression("Relative Spawning Biomass (" ~ B[t] ~ "/" ~ B[0] ~ ")"),
               xlab="Year", main=title, las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], 0, yUpper, first=FALSE, opacity=opacity)
    }
  }

  # Envelope done, now plot USR and LRP with credible intervals
  bmsy <- out[[1]]$mcmc$params$bmsy / out[[1]]$mcmc$params$bo
  bmsy4 <- 0.4 * bmsy
  bmsy8 <- 0.8 * bmsy
  bmsy4quants <- getQuants(bmsy4, ci)
  bmsy8quants <- getQuants(bmsy8, ci)
  bmsy4shade <- .getShade("red", opacity)
  bmsy8shade <- .getShade("orange", opacity)
  # Plot median line and credible interval for 0.4Bmsy
  polygon(c(yrs,rev(yrs)), c(rep(bmsy4quants[1], length(yrs)), rep(bmsy4quants[3], length(yrs))), col=bmsy4shade, border="red")
  lines(c(yrs[1], yrs[length(yrs)]), c(bmsy4quants[2], bmsy4quants[2]), col="red", lty=2, lwd=2)

  # Plot median line and credible interval for 0.8Bmsy
  polygon(c(yrs,rev(yrs)), c(rep(bmsy8quants[1], length(yrs)), rep(bmsy8quants[3], length(yrs))), col=bmsy8shade, border="orange")
  lines(c(yrs[1], yrs[length(yrs)]), c(bmsy8quants[2], bmsy8quants[2]), col="orange", lty=2, lwd=2)

  legend(leg, legend=c(expression("USR: 0.8B"[msy]), expression("LRP: 0.4B"[msy])), fill=c(bmsy8shade, bmsy4shade), border=c("orange","red"))

  # Add 0.2B0 and 0.4B0 static lines
  abline(h=0.2, col="black", lty=2, lwd=2)
  abline(h=0.4, col="black", lty=2, lwd=2)
  # Add labels for the static lines on the right of the plot
  mtext(expression(" 0.2B"[0]), side=4, at=0.2, las=1)
  mtext(expression(" 0.4B"[0]), side=4, at=0.4, las=1)
}

plotVBiomassMPD <- function(out       = NULL,
                            colors    = NULL,
                            names     = NULL,
                            lty       = NULL,
                            verbose   = FALSE,
                            showtitle = TRUE,
                            leg = "topright",
                            showSbio  = FALSE,
                            add       = FALSE,
                            opacity   = 90){
  # Vulnerable biomass plot for an MPD
  # out is a list of the mpd outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # showSbio is whether or not to add the spawning biomass to the plot
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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  # For vbt from the report file, 1st column is gear number, 2nd column is group, 3rd is year, and 4th is vbt
  # This code will assume only one group, and will ignore the 2nd column
  # Also, only one gear is assumed, which is gear==1 in the first column

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

  sbt <- out[[1]]$mpd$sbt
  vbt <- out[[1]]$mpd$vbt
  vbt <- vbt[vbt[,1]==1,] # Filters only gear #1
  if(showSbio){
    yUpper <- max(vbt[,4],sbt)
  }else{
    yUpper <- max(vbt[,4])
  }
  for(model in 1:length(out)){
    sbt <- out[[model]]$mpd$sbt
    vbt <- out[[model]]$mpd$vbt
    vbt <- vbt[vbt[,1]==1,] # Filters only gear #1
    if(showSbio){
      yUpper <- max(yUpper, vbt[,4], sbt)
    }else{
      yUpper <- max(yUpper, vbt[,4])
    }
  }
  #par(mar=c(3,6,3,3))
  title <- ""
  if(showtitle){
    if(showSbio){
      title <- "Vulnerable vs. Spawning Biomass"
    }else{
      title <- "Vulnerable Biomass"
    }
  }
  sbt <- out[[1]]$mpd$sbt
  vbt <- out[[1]]$mpd$vbt
  vbt <- vbt[vbt[,1]==1,] # Filters only gear #1
  yrs <- vbt[,3]
  vbt <- vbt[,4]
  plot(yrs, vbt, type="l", col=colors[[1]], lty=lty[[1]], lwd=2,ylim=c(0,yUpper),ylab="Biomass (1000 mt)\n", xlab="Year", main=title, las=1)
  if(length(out) == 1 && showSbio){
    # Make the spawning biomass the same color as the vulnerable, but up one linetype
    lines(yrs, sbt, type="l", col=colors[[1]], lty=lty[[1]]+1, lwd=2,ylim=c(0,yUpper),ylab="Biomass (1000 mt)\n", xlab="Year", main=title, las=1)
    vbioname <- paste0("VBio - ",names[[1]])
    sbioname <- paste0("Sbio - ",names[[1]])
    names[[1]] <- vbioname
    names[[2]] <- sbioname
    lty[[2]] <- lty[[1]] + 1
    colors[[2]] <- colors[[1]]
  }
  if(length(out) > 1){
    for(line in 2:length(out)){
      vbt <- out[[line]]$mpd$vbt
      vbt <- vbt[vbt[,1]==1,] # Filters only gear #1
      yrs <- vbt[,3]
      vbt <- vbt[,4]
      lines(yrs, vbt, type="l", col=colors[[line]], lty=lty[[line]], lwd=2, ylim=c(0,yUpper))
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }
}

plotVBiomassMCMC <- function(out       = NULL,
                             colors    = NULL,
                             names     = NULL,
                             ci        = NULL,
                             burnthin  = list(0,1),
                             verbose   = FALSE,
                             showtitle = TRUE,
                             leg = "topright",
                             showSbio  = FALSE,
                             add       = FALSE,
                             opacity   = 90){
  # Vulnerable biomass plot for an MCMC
  # out is a list of the mcmc outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # ci is the confidence interval to use in percent, eg. 95
  # showSbio is whether or not to add the spawning biomass to the plot

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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  squants <- vector("list", length(out))
  vquants <- vector("list", length(out))
  for(model in 1:length(out)){
    sbt <- window(mcmc(out[[model]]$mcmc$sbt[[1]]), start=burn, thin=thin)
    vbt <- window(mcmc(out[[model]]$mcmc$vbt[[1]][[1]]), start=burn, thin=thin)
    squants[[model]] <- getQuants(sbt, ci)
    vquants[[model]] <- getQuants(vbt, ci)
  }
  yUpper <- 0
  for(model in 1:length(out)){
    if(showSbio){
      yUpper <- max(yUpper, squants[[model]], vquants[[model]])
    }else{
      yUpper <- max(yUpper, vquants[[model]])
    }
  }
  syrs <- as.numeric(names(out[[1]]$mcmc$sbt[[1]]))
  vyrs <- syrs[1:length(syrs)]
  #par(mar=c(3,6,3,3))
  title <- ""
  if(showtitle){
    if(showSbio){
      title <- "Vulnerable vs. Spawning Biomass"
    }else{
      title <- "Vulnerable Biomass"
    }
  }
  if(length(out) == 1){
    if(showSbio){
      # Make the spawning biomass the same linetype as the vulnerable, but up one color
      drawEnvelope(syrs, squants[[1]], colors[[1]] + 1, 0, yUpper, first=TRUE, opacity=opacity, ylab="Biomass (1000 mt)\n", xlab="Year", main=title, las=1)
      drawEnvelope(vyrs, vquants[[1]], colors[[1]], 0, yUpper, first=FALSE, opacity=opacity, ylab="Biomass (1000 mt)\n", xlab="Year", main=title, las=1)
      vbioname <- "Vulnerable B"
      sbioname <- "Spawning B"
#      vbioname <- paste0("VBio - ",names[[1]])
#      sbioname <- paste0("Sbio - ",names[[1]])
      names[[1]] <- vbioname
      names[[2]] <- sbioname
      colors[[2]] <- colors[[1]] + 1
    }else{
      drawEnvelope(vyrs, vquants[[1]], colors[[1]], 0, yUpper, first=TRUE, opacity=opacity, ylab="Biomass (1000 mt)\n", xlab="Year", main=title, las=1)
    }
  }
  if(length(out) > 1 && !showSbio){
    for(line in 1:length(out)){
      vbt <- window(mcmc(out[[model]]$mcmc$vbt[[1]][[1]]), start=burn, thin=thin)
      vquants[[model]] <- getQuants(vbt, ci)
      if(line==1){
        drawEnvelope(vyrs, vquants[[line]], colors[[line]], 0, yUpper, first=TRUE, opacity=opacity)
      }else{
        drawEnvelope(vyrs, vquants[[line]], colors[[line]], 0, yUpper, first=FALSE, opacity=opacity)
      }
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
}




########################################

plotRecruitmentMPD <- function(out       = NULL,
                               colors    = NULL,
                               names     = NULL,
                               lty       = NULL,
                               verbose   = FALSE,
                               showtitle = TRUE,
                               add       = FALSE,
                               recrShowFinalYear = TRUE,
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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

  sage   <- out[[1]]$mpd$sage
  nyear  <- length(out[[1]]$mpd$yr)
  ryr    <- out[[1]]$mpd$yr[(1+sage):nyear]
  rt     <- out[[1]]$mpd$rt
  if(!recrShowFinalYear){
    ryr <- ryr[-length(ryr)]
    rt <- rt[-length(rt)]
  }
  yUpper <- max(rt)
  for(model in 1:length(out)){
    tmprt  <- out[[model]]$mpd$rt
    if(!recrShowFinalYear){
      tmprt <- tmprt[-length(tmprt)]
    }
    yUpper <- max(yUpper, tmprt)
  }
  xlim <- c(min(ryr), max(ryr))
  if(length(out)>1){
    for(model in 1:length(out)){
      tmpsage  <- out[[model]]$mpd$sage
      tmpnyear <- length(out[[model]]$mpd$yr)
      tmpryr   <- out[[model]]$mpd$ryr[(1+tmpsage):nyear]
      if(!recrShowFinalYear){
        tmpryr <- tmpryr[-length(tmpryr)]
        tmprt <- tmprt[-length(tmprt)]
      }
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
      if(!recrShowFinalYear){
        ryr <- ryr[-length(ryr)]
        rt <- rt[-length(rt)]
      }
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
                                add       = FALSE,
                                recrShowFinalYear = TRUE,
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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
   #quants[[model]] <- getQuants(out[[model]]$mcmc$rt[[1]], ci)
   rt <- window(mcmc(out[[model]]$mcmc$rt[[1]]), start=burn, thin=thin)
   if(!recrShowFinalYear){
     rt <- rt[,-ncol(rt)]
   }
   quants[[model]] <- getQuants(rt, ci)
  }
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yUpper <- max(yUpper, quants[[model]])
  }

  yrs <- as.numeric(names(out[[1]]$mcmc$rt[[1]]))
  if(!recrShowFinalYear){
    yrs <- yrs[-length(yrs)]
  }

  #RF - need to get xlim in case time series lengths differ
  Xlim <- c(min(yrs), max(yrs))
  if(length(out)>1){
    for(model in 1:length(out)){
      tmpryr <- as.numeric(names(out[[model]]$mcmc$rt[[1]]))
      if(!recrShowFinalYear){
        tmpryr <- tmpryr[-length(tmpryr)]
      }
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
      if(!recrShowFinalYear){
        yrs <- yrs[-length(yrs)]
      }
      # Plot the uncertainty
      points(yrs+incOffset, quants[[line]][2,], pch=20, col=colors[[line]])
      arrows(yrs+incOffset, quants[[line]][1,],
             yrs+incOffset, quants[[line]][3,], col=colors[[line]], code=3, angle=90, length=0.01)
      incOffset <- incOffset + offset
    }
  }
  if(length(out) == 1){
    abline(h=median(as.matrix(rt)),col=2, lty=1)
    abline(h=mean(as.matrix(rt)),col=3,lty=1)
    if(!is.null(leg))  legend(leg, legend=c(names,"Long-term median","MCMC long-term mean"), col=c(unlist(colors),2,3), lty=1, lwd=2)
  }
  if(length(out) > 1){
    if(!is.null(leg))  legend(leg, legend=names, col=unlist(colors), lty=1, lwd=2)
  }

}

plotRecruitmentDevsMCMC <- function(out       = NULL,
                                    colors    = NULL,
                                    names     = NULL,
                                    ci        = NULL,
                                    burnthin  = list(0,1),
                                    offset    = 0.1,
                                    verbose   = FALSE,
                                    showtitle = TRUE,
                                    add       = FALSE,
                                    leg = "topright"){
  # Recruitment deviations plot for an MCMC
  # out is a list of the mcmc outputs to show on the plot
  # colors is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # ci is the confidence interval in percent, eg 95
  # offset is the number of years to offset the points and bars
  #  for clarity on the plot, i.e. so that there is no overlapping.
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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

  # Calculate quantiles for the posterior data if an MCMC is to be plotted
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
    rdev <- window(mcmc(out[[model]]$mcmc$rdev[[1]]), start=burn, thin=thin)
    quants[[model]] <- getQuants(rdev, ci)
  }
  yLower <- min(quants[[1]])
  yUpper <- max(quants[[1]])
  for(model in 1:length(out)){
    yLower <- min(yLower, quants[[model]])
    yUpper <- max(yUpper, quants[[model]])
  }

  yrs <- as.numeric(names(out[[1]]$mcmc$rdev[[1]]))
  title <- ""
  if(showtitle){
    title <- "Recruitment Deviations"
  }
	plot(yrs, quants[[1]][2,], type="p", pch=20, col=colors[[1]], ylim=c(yLower,yUpper), xlab="Year", ylab="Log recruitment deviations", main=title, las=1)
  	arrows(yrs, quants[[1]][1,], yrs, quants[[1]][3,], col=colors[[1]], code=3, angle=90, length=0.01)
		  #drawEnvelope(yrs, quants[[1]], colors[[1]], yLower, yUpper, first=TRUE, opacity=opacity, ylab="Recruitment Deviations", xlab="Year", main=title, las=1)
		  #if(length(out) > 1){
		   # for(line in 2:length(out)){
		   #   drawEnvelope(yrs, quants[[line]], colors[[line]], yLower, yUpper, first=FALSE, opacity=opacity)
		   # }
                    #}
  	 if(length(out) > 1){
	     incOffset <- offset
	     for(line in 2:length(out)){
	       points(yrs+incOffset, quants[[line]][2,], pch=20, col=colors[[line]])
	       arrows(yrs+incOffset, quants[[line]][1,], yrs+incOffset, quants[[line]][3,], col=colors[[line]], code=3, angle=90, length=0.01)
     		 incOffset <- incOffset + offset
  	   }
	  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=1, lwd=2)
  }
}

plotRecruitmentDevsMPD <- function(out       = NULL,
                                   parout    = NULL,
                                   colors    = NULL,
                                   names     = NULL,
                                   lty       = NULL,
                                   verbose   = FALSE,
                                   showtitle = TRUE,
                                   add       = FALSE,
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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

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
       ylab="Log recruitment deviations (millions)", xlab="Year", main=title, las=1)
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

plotIndexMPD <- function(scenario   = NULL,
                         out        = NULL,
                         inputs     = NULL,
                         index      = NULL,
                         colors     = NULL,
                         names      = NULL,
                         lty        = NULL,
                         verbose    = FALSE,
                         showtitle  = TRUE,
                         leg        = "topright",
                         add        = FALSE,
                         indfixaxis = FALSE){
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
  # indfixaxis, if TRUE then all index plots will be scaled to the overall year span of all indices

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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  ## For future: No assumption of first model having all indices
  # Get the names of all models' input indices so that the plot will work
  # even if some indices are not included in the first one.
  ## indexnames <- NULL
  ## for(model in 1:length(inputs)){
  ##   indices <- inputs[[model]]$indices
  ##   gearnames <- inputs[[model]]$gearNames
  ##   for(ind in 1:length(indices)){
  ##     indexmat <- as.data.frame(indices[[ind]])
  ##     gearindex <- unique(indexmat$gear)
  ##     indexnames <- c(indexnames, gearnames[gearindex])
  ##   }
  ## }
  ## indexnames <- unique(indexnames)
  # At this point, indexnames holds the names of all indices used in
  # all models. Each name is unique in the vector.

  # Get index names included in the model
  indices <- inputs[[1]]$indices
  gearindices <- NULL
  for(ind in 1:length(indices)){
    indexmat <- as.data.frame(indices[[ind]])
    gearindices <- c(gearindices, unique(indexmat$gear))
  }
  if(!is.element(index,gearindices)){
    cat0(.PROJECT_NAME,"->",currFuncName,"That gear does not have an index in the model.")
    return(NULL)
  }

  currindexname <- inputs[[1]]$gearNames[index]
  mat <- NULL
  for(model in 1:length(out)){
    # For each model, match the data with the gear name 'currgearname'
    # If it does not match, it will be skipped
    indexnames <- inputs[[1]]$gearNames[index]
    gearnum <- match(currindexname, inputs[[1]]$gearNames)
    if(is.na(gearnum)){
      # Remove the gear from the legend lists, using the property that if a list
      # element is set to NULL, it will be removed completely from the list.
      lty[[model]] <- NA
      colors[[model]] <- NA
      names[[model]] <- NA
    }else{
      # Get correct ind for the given gear
      indices <- inputs[[model]]$indices
      gearindreal <- NA
      for(ind in 1:length(indices)){
        indexmat <- as.data.frame(indices[[ind]])
        gearindex <- unique(indexmat$gear)
        if(gearindex == gearnum){
          gearindreal <- ind
        }
      }
      if(!is.na(gearindreal)){
        inputindices <- as.data.frame(inputs[[model]]$indices[[gearindreal]])
        outputit     <- as.data.frame(out[[model]]$mpd$it_hat)
        if(ncol(outputit) == 1){
          dat          <- as.numeric(outputit[,1])
        }else{
          dat          <- as.numeric(outputit[gearindreal,])
        }
        tmpindices   <- as.data.frame(inputs[[model]]$indices[[gearindreal]])
        yrs          <- tmpindices$iyr
        cv           <- 1 / inputindices$wt
        dat          <- dat[!is.na(dat)]
        mat          <- cbind(mat, dat)
      }else{
        # Remove the gear from the legend lists, using the property that if a list
        # element is set to NULL, it will be removed completely from the list.
        lty[[model]] <- NA
        colors[[model]] <- NA
        names[[model]] <- NA
      }
    }
  }
  title <- ""
  if(showtitle){
    title <- paste0("Index fit - ",currindexname)
  }
  # Change the NAs to NULLs for the legend variables, using the property that if a list
  # element is set to NULL, it will be removed completely from the list.
  lty[sapply(lty, is.na)] <- NULL
  colors[sapply(colors, is.na)] <- NULL
  names[sapply(names, is.na)] <- NULL
  # If user requests a fixed scale x-axis, go through all indices to get year range
  if(indfixaxis){
    xmin <- NULL
    xmax <- NULL
    for(ind in 1:length(indices)){
      xmin <- min(xmin, indices[[ind]][,1])
      xmax <- max(xmax, indices[[ind]][,1])
    }
    ymax <- max(inputindices$it + cv * inputindices$it)
    xlim <- c(xmin, xmax)
    ylim <- c(0, ymax)
    matplot(yrs, mat, type = "l", lwd = 2, lty = unlist(lty), col = unlist(colors),
            las = 1, main = title, xlim = xlim, ylim = ylim, xlab="",
            ylab="", axes=FALSE)
  }else{
    xmin <- min(yrs)
    xmax <- max(yrs)
    ymax <- max(inputindices$it + cv * inputindices$it)
    xlim <- c(xmin, xmax)
    ylim <- c(0, ymax)
    matplot(yrs, mat, type = "l", lwd = 2, lty = unlist(lty), col = unlist(colors),
            las = 1, main = title, ylim = ylim,
            xlab="", ylab="", axes=FALSE)
    #axis(1, at = yrs)
    #axis(2, at = seq(0, max(mat, inputindices$it + cv * inputindices$it), by = max(mat, inputindices$it + cv * inputindices$it) / 10))
  }
  points(yrs, inputindices$it, pch = 3)
  arrows(yrs, inputindices$it + cv * inputindices$it ,yrs, inputindices$it - cv * inputindices$it,
         code = 3, angle = 90, length = 0.01, col = "black")

  axis(1, at     = seq(min(xlim),max(xlim)),
          labels = seq(min(xlim),max(xlim)))
  axis(2)
  box()
  mtext("Year", 1, line=2)
  mtext("x 1,000 tonnes", 2, line=2)

  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2, y.intersp=1)
  }
}

plotFMPD <- function(out       = NULL,
                     colors    = NULL,
                     names     = NULL,
                     pch       = 20,
                     pointSize = 0.2,
                     verbose   = FALSE,
                     showtitle = TRUE,
                     leg       = "topright",
                     plotU     = FALSE,
                     add       = FALSE,
                     opacity   = 90){
  # Fishing mortality plot for an MPD
  # out is a list of the mpd outputs to show on the plot
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # pch and pointsize are passed to the plot function. pointSize is in fact 'cex'
  # plotU if TRUE plot Ut, if FALSE, plot Ft
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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

  nsex   <- out[[1]]$mpd$nsex
  yrs    <- out[[1]]$mpd$yr
  nyear  <- length(yrs)
  ngear  <- out[[1]]$mpd$ngear

  # This is the F with the # of rows being the number of gears, and that is multiplied
  # by the number of sexes. So for a 3-gear, 2 sex model, there will be 6 rows in f with main grouping
  # by gear, i.e. two, three-row groupings.
  numModels <- length(out)
  f         <- out[[1]]$mpd$ft
  if(plotU){
    # U is always between 0 and 1, and f will become U with the transformation
    f <- 1.0 - exp(-f)
    yUpper <- 1.1
  }else{
    yUpper    <- max(f)
    for(model in 1:length(out)){
      yUpper <- max(yUpper, out[[model]]$mpd$ft)
    }
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
          if(plotU){
            ylabel <- "U"
          }else{
            ylabel <- "F"
          }
          plot(yrs, meanF, type = "o", col=color, pch=pch, cex=pointSize, lty=sex, lwd=2, ylim=c(0,yUpper), ylab=ylabel, xlab="Year", main=title, las=1)
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
      if(plotU){
        # U is always between 0 and 1, and f will become U with the transformation
        f <- 1.0 - exp(-f)
      }
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
                      leg       = "topright",
                      plotU     = FALSE,
                      add       = FALSE,
                      opacity   = 90){
  # Fishing mortality plot for mcmc models
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # ci is the confidence interval to use in percent, eg. 95
  # pch and pointsize are passed to the plot function. pointSize is in fact 'cex'
  # plotU if TRUE plot Ut, if FALSE, plot Ft

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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  # This is required on biomass plots to make the y-axis label visible, only 2nd item is +1 from default
  par(mar=c(5.1,5.1,4.1,2.1))

  # Calculate quantiles for the posterior data
  quants <- vector("list", length(out))
  for(model in 1:length(out)){
    # the following has a f[[1]]. The correct way would be to have iscam only output gear 1 instead of all gears,
    # similar to sbt output.
    f <- out[[model]]$mcmc$ft[[1]][[1]]
    if(plotU){
      # U is always between 0 and 1, and f will become U with the transformation
      f <- 1.0 - exp(-f)
    }
    ft <- window(mcmc(f), start=burn, thin=thin)
    quants[[model]] <- getQuants(ft, ci)
  }
  if(plotU){
    yUpper <- 1.1
  }else{
    yUpper <- max(quants[[1]])
    for(model in 1:length(out)){
      yUpper <- max(yUpper, quants[[model]])
    }
  }
  # Get last four digits of the names
  ynames <- names(out[[1]]$mcmc$ft[[1]][[1]])
  pattern <- ".*_([0-9]+)"
  yrs <- as.numeric(sub(pattern,"\\1",ynames))
  #par(mar=c(3,6,3,3))
  title <- ""
  if(showtitle){
    title <- "Fishing Mortality"
  }
  if(plotU){
    ylabel <- "U"
  }else{
    ylabel <- "F"
  }
  drawEnvelope(yrs, quants[[1]], colors[[1]], 0, yUpper, first=TRUE, opacity=opacity, ylab=ylabel, xlab="Year", main=title, las=1)
  if(length(out) > 1){
    for(line in 2:length(out)){
      drawEnvelope(yrs, quants[[line]], colors[[line]], 0, yUpper, first=FALSE, opacity=opacity)
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
                                    leg       = "topright",
                                    shortnames= NULL,
                                    add       = FALSE,
                                    showumsy  = FALSE){
  # Reference points plot for an MCMC model
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # ci is the confidence interval to use in percent, eg. 95
  # pch and pointsize are passed to the plot function. pointSize is in fact 'cex'
  # showumsy if TRUE will plot Umsy instead of Fmsy
  # shortnames if not null will be used instead of full names
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
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  bo   <- as.vector(window(mcmc(out[[1]]$mcmc$params$bo), start=burn, thin=thin))
  bmsy <- as.vector(window(mcmc(out[[1]]$mcmc$params$bmsy), start=burn, thin=thin))
  msy  <- as.vector(window(mcmc(out[[1]]$mcmc$params$msy1), start=burn, thin=thin))
  fmsy <- as.vector(window(mcmc(out[[1]]$mcmc$params$fmsy1), start=burn, thin=thin))
  umsy <- as.vector(window(mcmc(out[[1]]$mcmc$params$umsy1), start=burn, thin=thin))

  if(length(out) > 1){
    for(model in 2:length(out)){
      bo   <- cbind(bo, as.vector(window(mcmc(out[[model]]$mcmc$params$bo), start=burn, thin=thin)))
      bmsy <- cbind(bmsy, as.vector(window(mcmc(out[[model]]$mcmc$params$bmsy), start=burn, thin=thin)))
      msy  <- cbind(msy, as.vector(window(mcmc(out[[model]]$mcmc$params$msy1), start=burn, thin=thin)))
      fmsy <- cbind(fmsy, as.vector(window(mcmc(out[[model]]$mcmc$params$fmsy1), start=burn, thin=thin)))
      umsy <- cbind(umsy, as.vector(window(mcmc(out[[model]]$mcmc$params$umsy1), start=burn, thin=thin)))
    }
  }

  colors <- c(do.call("cbind",colors)) # Convert colors list to vector
  names  <- c(do.call("cbind",names))
  if(!is.null(shortnames)){
    names <- shortnames
  }

  par(mfrow=c(2,2), mai=c(0.3,0.5,0.4,0.2), oma=c(1.,1.2,0.2,0.1))
  if(showumsy){
    ymax <- 1.1
    boxplot(umsy, pch=pch, range = ci/100, names=names, border=colors, main="UMSY", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,ymax))
  }else{
    ymax <- max(fmsy)
    boxplot(fmsy, pch=pch, range = ci/100, names=names, border=colors, main="FMSY", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,ymax))
  }
  ymax <- max(msy)
  boxplot(msy, pch=pch, range = ci/100, names=names, border=colors, main="MSY (1000mt)", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,ymax))
  ymax <- max(bo)
  boxplot(bo, pch=pch, range = ci/100, names=names, border=colors, main="B0 (1000mt)", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,ymax))
  ymax <- max(bmsy)
  boxplot(bmsy, pch=pch, range = ci/100, names=names, border=colors, main="BMSY (1000mt)", las=1, cex.axis=1.2, cex=1.2, ylim=c(0,ymax))
}

plotIndexData <- function(scenario   = NULL,
                          index      = NULL,
                          showtitle  = TRUE,
                          indfixaxis = FALSE){
  # Index data plot
  # scenario is the scenario number.
  # col is a list of the colors to use in the plot
  # names is a list of the names to use in the legend
  # Notes:
  # - Models may have different gears than others, but we want the index plots to match by gear.
  #   The solution is to match them by name if plotting multiple (sensitivity plots)
  #   by creating a unique vector of names which is the union of all names across all models
  #   and using that to match to the names in each model, only plotting if the name is found.
  # indfixaxis, if TRUE then all index plots will be scaled to the overall year span of all indices

  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  # Get index names included in the model
  inputs <- op[[1]]$inputs$data
  indices <- inputs$indices
  gearindices <- NULL
  for(ind in 1:length(indices)){
    indexmat <- as.data.frame(indices[[ind]])
    gearindices <- c(gearindices, unique(indexmat$gear))
  }
  if(!is.element(index,gearindices)){
    cat0(.PROJECT_NAME,"->",currFuncName,"That gear does not have an index in the model.")
    return(NULL)
  }
  currindexname <- inputs$gearNames[index]
  mat <- NULL
  # For each model, match the data with the gear name 'currgearname'
  # If it does not match, it will be skipped
  indexname <- inputs$gearNames[index]
  gearnum <- match(currindexname, inputs$gearNames)
  if(is.na(gearnum)){
    cat0(.PROJECT_NAME,"->",currFuncName,"That gear does not have an index in the model.")
    return(NULL)
  }else{
    indices <- inputs$indices
    gearindreal <- NA
    for(ind in 1:length(indices)){
      indexmat <- as.data.frame(indices[[ind]])
      gearindex <- unique(indexmat$gear)
      if(gearindex == gearnum){
        gearindreal <- ind
      }
    }
    if(!is.na(gearindreal)){
      inputindices <- as.data.frame(indices[[gearindreal]])
      yrs          <- inputindices$iyr
      cv           <- 1 / inputindices$wt
    }else{
      cat0(.PROJECT_NAME,"->",currFuncName,"That gear does not have an index in the model.")
      return(NULL)
    }
  }
  title <- ""
  if(showtitle){
    title <- paste0("Index fit - ",currindexname)
  }
  # If user requests a fixed scale x-axis, go through all indices to get year range
  ymin <- 0
  ymax <- NULL
  if(indfixaxis){
    xmin <- NULL
    xmax <- NULL
    for(ind in 1:length(indices)){
      xmin <- min(xmin, indices[[ind]][,1])
      xmax <- max(xmax, indices[[ind]][,1])
    }
    ymax <- max(inputindices$it + cv * inputindices$it)
  }else{
    xmin <- min(yrs)
    xmax <- max(yrs)
    ymax <- max(inputindices$it + cv * inputindices$it)
  }
  xlim <- c(xmin, xmax)
  ylim <- c(ymin, ymax)
  plot(yrs, inputindices$it, type="p", pch = 3, ylim=ylim, xlim=xlim, ylab="", xlab="", axes=FALSE)
  arrows(yrs, inputindices$it + cv * inputindices$it ,yrs, inputindices$it - cv * inputindices$it,
         code = 3, angle = 90, length = 0.01, lwd=2, col = "black")
  axis(1, at     = seq(min(xlim),max(xlim)),
          labels = seq(min(xlim),max(xlim)))
  axis(2)
  box()
  mtext("Year", 1, line=2)
  mtext("x 1,000 tonnes", 2, line=2)
}
