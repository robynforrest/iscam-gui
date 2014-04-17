#**********************************************************************************
# iscam-gui-figures-catch.r
# This file contains the code for catch values iscam inputs using the
# infrastructure provided with iscam-gui.
#
# Author            : Chris Grandin
# Development Date  : October 2013 - Present
#**********************************************************************************

plotCatch <- function(plotNum  = 1,
                      fileText = "Default",
                      units    = .UNITS,
                      png      = .PNG,
                      silent   = .SILENT){

  # Assumes that 'op' list exists and has been populated correctly.
  # plotNum must be one of:
  # 1  Landings
  # 2  SPR status, or the spawning potential ratio:
  #    (1-spr)/(1-spr.at.msy)

  currFuncName <- getCurrFunc()
  val          <- getWinVal()
  scenario     <- val$entryScenario
  scenarioName <- op[[scenario]]$names$scenario
  inp          <- op[[scenario]]$inputs$data
  inputs       <- op[[scenario]]$inputs
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
  if(plotNum == 1){
    plotCatches(inp = inputs, scenarioName, legendLoc = legendLoc, col = color)
  }
  if(plotNum == 2){
    plotSPR(inp = inputs, scenarioName, legendLoc = legendLoc, col = color)
  }

  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

plotCatches <- function(inp,
                        scenarioName,
                        verbose = FALSE,
                        legendLoc = "topright",
                        col = 1){
  # Catch plot for iscam model
  catch <- inp$data$catch
  years <- catch[,"year"]
  value <- catch[,"value"]
	barplot(value, names.arg=years, xlab="Year", ylab="Landings", las=1, col = col)
}

plotSPR <-  function(inp,
                     scenarioName,
                     verbose = FALSE,
                     legendLoc = "topright",
                     col = 1){

}
