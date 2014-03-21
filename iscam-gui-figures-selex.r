#**********************************************************************************
# ss-explore-figures-selex.r
# This file contains the code for plotting selectivity values SS outputs using the
# infrastructure provided with ss-explore.
#
# Author            : Chris Grandin
# Development Date  : October 2013
# Current version   : 1.0
#**********************************************************************************

plotSelex <- function(plotNum  = 1,
                      fileText = "Default",
                      units    = .UNITS,
                      png      = .PNG,
                      silent   = .SILENT){

  # plotNum must be one of:
  # 1  Length-based selectivity in end year by fleet
  # 2  Age-based selectivity in end year by fleet
  # 3  Selectivity at length time-varying surface
  # 4  Selectivity at length time-varying contour
  # 5  Retention at length time-varying surface
  # 6  Retention at length time-varying surface
  # 7  Discard mortality time-varying surface
  # 8  Discard mortality time-varying contour
  # 9  Selectivity, retention, and discard mortality at length in ending year
  # 10 NOT USED
  # 11 Selectivity at age time-varying surface
  # 12 Selectivity at age time-varying contour
  # 13 Selectivity at age in ending year if time-varying
  # 14 Selectivity at age in ending year if NOT time-varying
  # 15 NOT USED
  # 16 NOT USED
  # 17 NOT USED
  # 18 NOT USED
  # 19 NOT USED
  # 20 NOT USED
  # 21 Selectivity at age and length contour with overlaid growth curve
  # 22 Selectivity with uncertainty if requested at end of control file

  if(plotNum < 1   ||
     plotNum > 22  ||
     plotNum == 10 ||
     plotNum == 15 ||
     plotNum == 16 ||
     plotNum == 17 ||
     plotNum == 18 ||
     plotNum == 19 ||
     plotNum == 20
     ){
    return(FALSE)
  }
  val      <- getWinVal()
  scenario <- val$entryScenario
  currFuncName <- getCurrFunc()
  isMCMC   <- op[[scenario]]$inputs$log$isMCMC
  figDir   <- op[[scenario]]$names$figDir
  out      <- op[[scenario]]$outputs$mpd
  res          <- val$entryResolution
  width        <- val$entryWidth
  height       <- val$entryHeight
  resScreen    <- val$entryResolutionScreen
  widthScreen  <- val$entryWidthScreen
  heightScreen <- val$entryHeightScreen

  filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,".png")
  filename     <- file.path(figDir,filenameRaw)
  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)
  }else{
    windows(width=widthScreen,height=heightScreen)
  }
  if(isMCMC){
   # plot mcmc model runs
  }else{
    # plot mpd model runs
  }

  # Generate an r4ss biology plots

  SSplotSelex(out,
              plot     = TRUE,
              print    = FALSE,
              subplot  = plotNum,
              pheight  = height,
              pwidth   = width,
              punits   = units,
              res      = res,
              verbose  =!silent)
  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}
