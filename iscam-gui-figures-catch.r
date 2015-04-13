#**********************************************************************************
# iscam-gui-figures-catch.r
# This file contains the code for catch values iscam inputs using the
# infrastructure provided with iscam-gui.
#
# Author            : Chris Grandin
# Development Date  : October 2013 - Present
#**********************************************************************************

plotCatch <- function(scenario   = 1,         # Scenario number
                      plotNum    = 1,         # Plot code number
                      savefig    = .SAVEFIG,  # TRUE/FALSE for plot output
                      fileText   = "Default", # Name of the file if png==TRUE
                      plotMCMC   = FALSE,     # TRUE/FALSE to plot MCMC output
                      ci         = NULL,      # confidence interval in % (0-100)
                      multiple   = FALSE,     # TRUE/FALSE to plot sensitivity cases
                      sensGroup  = 1,         # Sensitivity group to plot if multiple==TRUE
                      index      = 1,         # Survey index to plot if plotNum==7
                      # PlotSpecs: Width, height, and resolution of screen and file
                      ps         = list(pngres = .RESOLUTION,
                                        pngw   = .WIDTH,
                                        pngh   = .HEIGHT,
                                        res    = .RESOLUTION,
                                        w      = .WIDTH,
                                        h      = .HEIGHT),
                      leg        = "topright",   # Legend location. If NULL, none will be drawn
                      from       = 1996,         # Start year for plotting catch
                      to         = 2014,         # End year for plotting catch
                      opacity    = 90,
                      areas = c("3C","3D","5A","5B","5C","5D","5E"), # Areas to plot
                      figtype    = .FIGURE_TYPE, # The filetype of the figure with period, e.g. ".png"
                      showtitle  = TRUE,         # Show the main title on the plot
                      units      = .UNITS,
                      silent     = .SILENT){

  # Assumes that 'op' list exists and has been populated correctly.
  # plotNum must be one of:
  # 1  Total catch (Landings + Discards)
  # 2  Landings / Discards seperated side-by-side barplot
  # 3  Catch fit
  # 4  Landings / Discards by area seperated side-by-side barplot
  currFuncName <- getCurrFunc()

  if(plotNum < 1 || plotNum > 4){
    cat0(.PROJECT_NAME,"->",currFuncName,"The plotNum must be between 1 and 10. You passed ",plotNum)
    return(FALSE)
  }

  scenarioName <- op[[scenario]]$names$scenario
  figDir       <- op[[scenario]]$names$figDir
  res          <- ps$pngres
  width        <- ps$pngw
  height       <- ps$pngh
  resScreen    <- ps$res
  widthScreen  <- ps$w
  heightScreen <- ps$h

  if(multiple){
    filenameRaw  <- paste0("SensitivityGroup_",sensGroup,"_",fileText,figtype)
    filename     <- file.path(.SENS_FIGURES_DIR_NAME,filenameRaw)
    # Extract models in the current sensitivity group
    if(is.null(sens[[sensGroup]])){
      cat0(.PROJECT_NAME,"->",currFuncName,"The sensitivity group you selected has no members.")
      return(NULL)
    }
    models <- sens[[sensGroup]]
  }else{
    filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,figtype)
    filename     <- file.path(figDir,filenameRaw)
    models <- scenario # For the non-multiple case
  }
  if(plotMCMC){
    # Remove models which do not have MCMC outputs
    type <- "mcmc"
    validModels <- getValidModelsList(models, type = type)
  }else{
    type <- "mpd"
    validModels <- getValidModelsList(models, type = type)
  }

  out    <- validModels[[1]]
  colors <- validModels[[2]]
  names  <- validModels[[3]]
  inputs <- validModels[[4]]
  linetypes <- validModels[[5]]

  if(is.null(validModels)){
    if(is.null(names)){
      cat0(.PROJECT_NAME,"->",currFuncName,"The model ",scenarioName," has no ",type," output associated with it.\n")
    }else{
      cat0(.PROJECT_NAME,"->",currFuncName,"The model ",names[[1]]," has no ",type," output associated with it.\n")
    }
    return(NULL)
  }

 if(savefig){
    graphics.off()
    if(figtype == .PNG_TYPE){
      png(filename,res=res,width=width,height=height,units=units)
    }
    if(figtype == .EPS_TYPE){
      setEPS(horizontal=FALSE, onefile=FALSE, paper="special",width=width,height=height)
      postscript(filename)
    }
  }else{
    windows(width=widthScreen,height=heightScreen)
  }

  if(plotNum == 1){
    plotCatches(leg = leg, col = color, showtitle = showtitle, from=from, to=to, opacity=opacity)
  }
  if(plotNum == 2){
    plotCatchesSplit(leg = leg, col = color, showtitle = showtitle, from=from, to=to, opacity=opacity)
  }
  if(plotNum == 3){
    plotCatchFit(inputs, out, colors=colors, lty=linetypes, names=names, scenarioName=scenarioName, leg = leg, showtitle = showtitle, opacity=opacity)
  }
  if(plotNum == 4){
    plotCatchesArea(leg = leg, col = color, showtitle = showtitle, from=from, to=to, areas=areas, opacity=opacity)
  }

  if(savefig){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

plotCatches_OLD <- function(inp,
                        scenarioName,
                        verbose = FALSE,
                        leg = "topright",
                        showtitle = TRUE,
                        col = 1){
  # Catch plot for iscam model, plots by gear
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  catch <- as.data.frame(inp$catch)
  p <- ggplot(catch,aes(x=factor(year),value,fill=factor(gear)))
	p <- p + geom_bar(width=0.75,position="dodge",stat="identity")
  p <- p + labs(x="Year",y="Catch (1000 mt)",fill="Gear")
  p <- p + .PLOT_THEME
  p <- p + theme(axis.text.x = element_text(angle = -90, hjust = 0))
	print(p)
}

plotSPR <-  function(inp,
                     scenarioName,
                     verbose = FALSE,
                     leg = "topright",
                        showtitle = TRUE,
                     col = 1){

}

plotCatchFit<-function(inp       = NULL,
                       out       = NULL,
                       colors    = NULL,
                       names     = NULL,
                       lty       = NULL,
                       scenarioName,
                       verbose   = FALSE,
                       leg       = "topright",
                       showtitle = TRUE,
                       opacity   = 90){
  # Catch fits plot, out contains a list of models to plot,
  # it must be at least length 1.
  # Assumes only one catch gear
  currFuncName <- getCurrFunc()
  if(is.null(inp)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an input vector (inp).")
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
  if(is.null(names)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a names vector (names).")
    return(NULL)
  }
  if(is.null(lty)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a linetypes vector (lty).")
    return(NULL)
  }

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  catchData <- as.data.frame(inp[[1]]$catch)
  years <- catchData$year
  obsCt <- catchData$value
  predCt <- out[[1]]$mpd$ct
  yUpper <- max(obsCt, predCt)

  for(model in 1:length(out)){
    yUpper <- max(yUpper, out[[model]]$mpd$ct)
  }

  xlim <- range(years)
  ylim <- c(0,yUpper)
  titletext <- ""
  if(showtitle){
    titletext <- "Catch fit"
  }
  plot(years, obsCt, pch=19, col=colors[[1]], lty=lty[[1]], xlim=xlim, ylim=ylim, type="p", xlab="Year", ylab="Catch (1000 mt)", main=titletext)
  lines(years, predCt, col=colors[[1]], lty=lty[[1]])
  if(length(out) > 1){
    for(line in 2:length(out)){
      catchData <- as.data.frame(inp[[line]]$catch)
      years <- catchData$year
      obsCt <- catchData$value
      xlim <- range(years)
      ylim <- c(0,yUpper)
      predCt <- out[[line]]$mpd$ct
      # Plot the obseverd catch points. Note these will be different for models with different areas
      points(years, obsCt, pch=19, col=colors[[line]], lty=lty[[line]], xlim=xlim, ylim=ylim, type="p", xlab="Year", ylab="Catch (1000 mt)", main=titletext)
      lines(years, predCt, col=colors[[line]], lty=lty[[line]])
    }
  }
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }

}

plotExpVsObsAnnualMeanWt<-function(inp,
                                out,
                                scenarioName,
                                verbose = FALSE,
                                leg = "topright",
                                showtitle = TRUE,
                                col = 1){
  nmeanwtObs <- inp$data$nmeanwtobs
  if( nmeanwtObs > 0){
		  meanwtData <- inp$data$meanwtdata
		  years <- meanwtData[,"year"]
		  obsMeanWt <- meanwtData[,"meanwt"]
		  gear <-meanwtData[,"gear"]
		  gearList<-unique(gear)
		  ngear<-length(gearList)	 #only plot for gears with data
		  predMeanWt <-out$annual_mean_weight

		  if (ngear==1) par(mfrow=c(1,1),mar=c(5,4,2,2))
		  if (ngear == 2) par(mfrow=c(2,1),mar=c(4,4,2,2))
		  if (ngear == 3 | ngear == 4) par(mfrow=c(2,2),mar=c(3,3,2,2))
		  if (ngear == 5 | ngear == 6) par(mfrow=c(3,2),mar=c(2,2,2,2))

		  for (i in 1:ngear) {
		      # Set-up plot area
		      xLim <- range(years)
		      yLim <- c(0,(max(obsMeanWt[gear==gearList[i]],predMeanWt[gear==gearList[i]])*1.1))

		      plot(xLim, yLim, type="n", axes=TRUE, xlab="Year", ylab="Mean Weight in Catch")

		      points(years[gear==gearList[i]], obsMeanWt[gear==gearList[i]], pch=19)
		      lines(years[gear==gearList[i]], predMeanWt[gear==gearList[i]], col="red")
		      box()
		  }
		  par(mfrow=c(1,1),mar=c(5,4,2,2))
	}else{
    cat0("WARNING: No Annual Mean Weight Data")
  }
}

plotCatches <- function(leg = "topright",
                        showtitle = TRUE,
                        col = 1,
                        from = 1996,         # Year to plot from
                        to   = 2014,         # Year to plot to
                        scalefactor = 1000,  # Divide the catch and discard by this factor
                        opacity = 90         # The transparency of the bars
                        ){
  # Catch plot for iscam model, plots by gear
  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  # Aggregate the data by year, with catch summed
  jcat <- catch[,-c(2:4,6:7)]
  jcat <- aggregate(catch$CatchKG, list(catch$Year), sum, na.rm=TRUE)

  # Aggregate the data by year, with discards summed
  dcat <- catch[,-c(2:5,7)]
  dcat <- aggregate(catch$DiscardedKG, list(catch$Year), sum, na.rm=TRUE)

  colnames(jcat) <- c("year","catch")
  colnames(dcat) <- c("year","catch")
  years <- jcat$year
  if(from < min(years) || to > max(years)){
    cat0(.PROJECT_NAME,"->",currFuncName,"The year range entered does not match the data. min = ",min(years),", max = ",max(years),".")
    return(NULL)
  }
  if(length(years) != length(dcat$year)){
    # This code doesn't take into account the situation where there were discards in a year but no catches
    cat0(.PROJECT_NAME,"->",currFuncName,"The catch and discards are mismatched. This probably means there were discards for one or more years when there was no catch.")
    return(NULL)
  }

  jcat <- jcat[jcat$year %in% from:to,]
  years <- years[years %in% from:to]
  jcat$catch <- jcat$catch / scalefactor

  dcat <- jcat[jcat$year %in% from:to,]
  dcat$catch <- jcat$catch / scalefactor

  # Sum the two amount by year
  totcatch <- cbind(years,jcat[,2]+dcat[,2])

  col1        <- .getShade(1, opacity)

  b <- barplot(totcatch[,2],
               axes=FALSE,
               col=c(col1),
               border=c("black"),
               ylim=c(0,1.1*max(totcatch[,2])),
               las=2)
  cex <- 0.7
  axis(2)
  axis(1,
       at     = b[,1],
       labels = years,
       tick = TRUE)
  box()
  xlabel <- "Year"
  ylabel <- "Catch (t)"

  mtext(side=1,line=2,xlabel)
  mtext(side=2,line=2,ylabel)

}

plotCatchesSplit <- function(leg = "topright",
                             showtitle = TRUE,
                             col = 1,
                             from = 1996,             # Year to plot from
                             to   = 2014,             # Year to plot to
                             scalefactor = 1000,      # Divide the catch and discard by this factor
                             spaceBetweenBars = 0.5,  # space between each year's set of bars
                             opacity = 90             # The transparency of the bars
                             ){
  # Catch plot for iscam model, plots by gear for landings and discards, split side-by-side bars
  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  # Aggregate the landings data by year, with catch and discards summed for total catch
  jcat <- catch[,-c(2:4,6:7)]
  jcat <- aggregate(catch$CatchKG, list(catch$Year), sum, na.rm=TRUE)

  # Aggregate the data by year, with discards summed
  dcat <- catch[,-c(2:5,7)]
  dcat <- aggregate(catch$DiscardedKG, list(catch$Year), sum, na.rm=TRUE)

  colnames(jcat) <- c("year","catch")
  colnames(dcat) <- c("year","catch")
  years <- jcat$year
  if(from < min(years) || to > max(years)){
    cat0(.PROJECT_NAME,"->",currFuncName,"The year range entered does not match the data. min = ",min(years),", max = ",max(years),".")
    return(NULL)
  }
  if(length(years) != length(dcat$year)){
    # This code doesn't take into account the situation where there were discards in a year but no catches
    cat0(.PROJECT_NAME,"->",currFuncName,"The catch and discards are mismatched. This probably means there were discards for one or more years when there was no catch.")
    return(NULL)
  }

  jcat <- jcat[jcat$year %in% from:to,]
  years <- years[years %in% from:to]
  jcat$catch <- jcat$catch / scalefactor

  dcat <- jcat[jcat$year %in% from:to,]
  dcat$catch <- jcat$catch / scalefactor

  # Make a matrix out of the two data series, and plot side-by-side
  allCatch <- as.matrix(cbind(jcat[,2],dcat[,2]))

  col1        <- .getShade(1, opacity)
  col2        <- .getShade(2, opacity)

  b <- barplot(t(allCatch),
               inset=c(-0.25,0),
               axes=FALSE,
               col=c(col1,col2),
               border=c("black","black"),
               ylim=c(0,1.1*max(allCatch)),
               las=2)

  cex <- 0.7
  axis(2)
  axis(1,
       at = b,
       labels = years,
       tick = TRUE)
  box()
  xlabel <- "Year"
  ylabel <- "Catch (t)"

  mtext(side=1,line=2,xlabel)
  mtext(side=2,line=2,ylabel)

  if(!is.null(leg)){
    legendList       <- c("Catch","Discards")
    legendShadeCols  <- c(col1,col2)
    legendBorderCols <- c("black","black")
    legend(leg,legendList,col=legendBorderCols,fill=legendShadeCols,bty="n")
  }
}

getAreaCodes <- function(areas = c("3C","3D","5A","5B","5C","5D","5E")){
  # return a vector of area codes correspponding to the areas given
  # 3 = 3C, 4 = 3D, 5 = 5A, 6 = 5B, 7 = 5C, 8 = 5D, 9 = 5E
  # Area hash table
  ahash <- matrix(ncol=2,nrow=7)
  ahash[1,] <- c(3,"3C")
  ahash[2,] <- c(4,"3D")
  ahash[3,] <- c(5,"5A")
  ahash[4,] <- c(6,"5B")
  ahash[5,] <- c(7,"5C")
  ahash[6,] <- c(8,"5D")
  ahash[7,] <- c(9,"5E")

  result <- ahash[ahash[,2] %in% areas,]
  if(class(result) == "character"){
    # areas was a single value
    ret <- result[1]
  }else{
    # areas was a vector
    ret <- result[,1]
  }
  return(as.numeric(ret))
}

plotCatchesArea <- function(leg = "topright",
                            showtitle = TRUE,
                            col = 1,
                            from = 1996,             # Year to plot from
                            to   = 2014,             # Year to plot to,
                            areas = c("3C","3D","5A","5B","5C","5D","5E"), # Areas to plot
                            spaceBetweenBars = 1.5,
                            scaleFactor = 1000, # The catch will be divided by this
                            opacity = 90 # The transparency of the bars
                            ){
  # Generates side-by-side barplot by area for the input catch and discard data
  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  #par(mar=c(4,4,1,1))


  # Filter for this area only
  areaCodes <- getAreaCodes(areas)
  if(length(areaCodes) == 0){
    cat0(.PROJECT_NAME,"->",currFuncName,"There was a problem with your area selection.")
    return(NULL)
  }

  # Filter the areas
  catch <- catch[catch$AreaCode %in% areaCodes,]

  # Remove month, day, area, and vessel id columns
  catch <- catch[,-c(2:4,7)]

  # Aggregate the data by year
  catches <- aggregate(catch$CatchKG, list(catch$Year), sum, na.rm=TRUE)
  discards <- aggregate(catch$DiscardedKG, list(catch$Year), sum, na.rm=TRUE)

  years <- catches[,1]

  if(from < min(years) || to > max(years)){
    cat0(.PROJECT_NAME,"->",currFuncName,"The year range entered does not match the data. min = ",min(years),", max = ",max(years),".")
    return(NULL)
  }
  if(length(years) != length(discards[,1])){
    # This code doesn't take into account the situation where there were discards in a year but no catches
    cat0(.PROJECT_NAME,"->",currFuncName,"The catch and discards are mismatched. This probably means there were discards for one or more years when there was no catch.")
    return(NULL)
  }

  catches <- catches[catches[,1] %in% from:to,]
  discards <- discards[discards[,1] %in% from:to,]
  years <- years[years %in% from:to]

  # Bind the catches and discards and apply the scale factor to both
  allcatch  <- apply(as.matrix(cbind(catches[,2], discards[,2])), c(1,2), function(x)x/scaleFactor)

  col1        <- .getShade(1, opacity)
  col2        <- .getShade(2, opacity)

  b <- barplot(t(allcatch),
               beside=TRUE,
               col=c(col1,col2),
               border=c("black","black"),
               space=c(0,spaceBetweenBars),
               axes=FALSE,
               ylim=c(0,1.1*max(allcatch)),
               las=2)

  cex <- 0.7
  xAxisSize <- length(years)*2 + spaceBetweenBars*(length(years)+1)
  axis(2)
  axis(1,
       at     = b[1,]+.25*spaceBetweenBars,
       labels = years,
       tick = TRUE)
  box()
  xlabel <- "Year"
  ylabel <- "Catch (t)"

  mtext(side=1,line=2,xlabel)
  mtext(side=2,line=2,ylabel)

  if(!is.null(leg)){
    legendList       <- c("Catch","Discards")
    legendShadeCols  <- c(col1,col2)
    legendBorderCols <- c("black","black")
    legend(leg,legendList,col=legendBorderCols,fill=legendShadeCols,bty="n")
  }
}

