#**********************************************************************************
# iscam-gui-figures-catch.r
# This file contains the code for catch values iscam inputs using the
# infrastructure provided with iscam-gui.
#
# Author            : Chris Grandin
# Development Date  : October 2013 - Present
#**********************************************************************************

plotCatch <- function(plotNum  = 1,
                      png        = .PNG,
                      fileText   = "Default",
                      plotMCMC   = FALSE,
                      ci         = NULL, # confidence interval in %
                      multiple   = FALSE,
                      units    = .UNITS,
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
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  if(plotNum < 1 || plotNum > 15){
    return(FALSE)
  }
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
  if(plotNum == 3){
    plotExpVsObsCatch(inp = inputs, out=out, scenarioName, legendLoc = legendLoc, col = color)
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
  # Catch plot for iscam model, plots by gear
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  catch <- as.data.frame(inp$data$catch)
  p <- ggplot(catch,aes(x=factor(year),value,fill=factor(gear)))
	p <- p + geom_bar(width=0.75,position="dodge",stat="identity")
  p <- p + labs(x="Year",y="Catch",fill="Gear")
  p <- p + .PLOT_THEME
  p <- p + theme(axis.text.x = element_text(angle = -90, hjust = 0))
	print(p)
}

plotSPR <-  function(inp,
                     scenarioName,
                     verbose = FALSE,
                     legendLoc = "topright",
                     col = 1){

}


plotExpVsObsCatch<-function(inp,
                                out,
                                scenarioName,
                                verbose = FALSE,
                                legendLoc = "topright",
                                col = 1){

  ngear<-inp$data$ngear
  catchData <- inp$data$catch
  years <- catchData[,"year"]
  obsCt <- catchData[,"value"]
  gear <-catchData[,"gear"]
  gearList<-unique(gear)
  predCt <-out$ct

  if (ngear==1) par(mfrow=c(1,1),mar=c(5,4,2,2))
  if (ngear == 2) par(mfrow=c(2,1),mar=c(4,4,2,2))
  if (ngear == 3 | ngear == 4) par(mfrow=c(2,2),mar=c(3,3,2,2))
  if (ngear == 5 | ngear == 6) par(mfrow=c(3,2),mar=c(2,2,2,2))

  for (i in 1:ngear) {
      # Set-up plot area
      xLim <- range(years)
      yLim <- c(0,(max(obsCt[gear==gearList[i]],predCt[gear==gearList[i]])*1.1))

      plot(xLim, yLim, type="n", axes=TRUE, xlab="Year", ylab="Index")

      points(years[gear==gearList[i]], obsCt[gear==gearList[i]], pch=19)
      lines(years[gear==gearList[i]], predCt[gear==gearList[i]], col="grey50")
      box()

  }

  par(mfrow=c(1,1),mar=c(5,4,2,2))

}
