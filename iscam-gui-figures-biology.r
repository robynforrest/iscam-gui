#**********************************************************************************
# ss-explore-figures-biology.r
# This file contains the code for plotting biological values SS outputs using the
# infrastructure provided with ss-explore.
#
# Author            : Chris Grandin
# Development Date  : August/September 2013
# Current version   : 1.0
#**********************************************************************************

plotBiology <- function(plotNum  = 1,
                        fileText = "Default", 
                        res      = .RESOLUTION,
                        width    = .WIDTH,
                        height   = .HEIGHT,
                        units    = .UNITS,
                        png      = .PNG,
                        silent   = .SILENT){

  # plotNum must be one of:
  # 1  Mean weight at length for last year
  # 2  Maturity at age
  # 3  Fecundity (only plotted if  all elements of biology$Fecundity are not NA)
  # 4  Fecundity as a function of weight (not always available)
  # 5  Fecundity as a function of length (not always available)
  # 6  Spawning output at length
  # 7  Ending year expected growth, length at age
  # 8  Time-varying natural mortality (only plotted if growthdatF$M vector contains different values)
  # 9  Perspective plot of time-varying growth (only plotted if growthvaries and growthseries are non-NULL)
  # 10 Contour plot of time-varying growth (only plotted if growthvaries and growthseries are non-NULL)

  if(plotNum < 1 || plotNum > 10){
    return(FALSE)
  }
  val          <- getWinVal()
  currFuncName <- getCurrFunc()
  scenario     <- val$entryScenario
  isMCMC       <- op[[scenario]]$inputs$log$isMCMC
  figDir       <- op[[scenario]]$names$figDir
  out          <- op[[scenario]]$outputs$mpd
  outMCMC      <- op[[scenario]]$outputs$mcmc
  filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,".png")
  filename     <- file.path(figDir,filenameRaw)
  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)

  }
  SSplotBiology(out,
                plot     = TRUE,
                print    = FALSE,
                subplots = plotNum,
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
