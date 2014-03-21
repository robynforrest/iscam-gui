#**********************************************************************************
# ss-explore-figures-catch.r
# This file contains the code for catch values SS outputs using the
# infrastructure provided with ss-explore.
#
# Author            : Chris Grandin
# Development Date  : October 2013
# Current version   : 1.0
#**********************************************************************************

plotCatch <- function(plotNum  = 1,
                      fileText = "Default",
                      res      = .RESOLUTION,
                      width    = .WIDTH,
                      height   = .HEIGHT,
                      units    = .UNITS,
                      png      = .PNG,
                      silent   = .SILENT){

  # plotNum must be one of:
  # 1  Landings
  # 2  Landings stacked
  # 3  Observed and expected landings
  # 4  Total catch (includinng discards)
  # 5  Total catch (including discards) stacked
  # 6  Discards
  # 7  Discards stacked
  # 8  Discard fraction
  # 9  Harvest rate
  # 10 Landings aggregated across seasons
  # 11 Landings aggregated across seasons stacked
  # 12 Total catch (if discards present) aggregated across seasons
  # 13 Total catch (if discards present) aggregated across seasons stacked
  # 14 Discards aggregated across seasons
  # 15 Discards aggregated across seasons stacked

  if(plotNum < 1 || plotNum > 15){
    return(FALSE)
  }
  val      <- getWinVal()
  scenario <- val$entryScenario
  isMCMC   <- op[[scenario]]$inputs$log$isMCMC
  figDir   <- op[[scenario]]$names$figDir
  out      <- op[[scenario]]$outputs$mpd

  filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,".png")
  filename     <- file.path(figDir,filenameRaw)
  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)

  }
  if(isMCMC){
   # plot mcmc model runs
  }else{
    # plot mpd model runs
  }

  # Generate an r4ss catch plot
  SSplotCatch(out,
              plot     = TRUE,
              print    = FALSE,
              subplot  = plotNum,
              pheight  = height,
              pwidth   = width,
              punits   = units,
              res      = res,
              verbose  = !silent)
  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}
