#**********************************************************************************
# ss-explore-figures-retrospectives.r
# This file contains the code necessary to plot Retrospective cohort (squid) plots
#
# Author            : Chris Grandin
# Development Date  : August 2014
#
#**********************************************************************************

plotCohorts <- function(scenario   = 1,         # Scenario number
                        png        = .PNG,      # TRUE/FALSE for PNG image output
                        fileText   = "Default", # Name of the file if png==TRUE
                        ylim       = c(-3,3),   # Limits for the recdevs
                        # PlotSpecs: Width, height, and resolution of screen and file
                        ps         = list(pngres = .RESOLUTION,
                                          pngw   = .WIDTH,
                                          pngh   = .HEIGHT,
                                          res    = .RESOLUTION,
                                          w      = .WIDTH,
                                          h      = .HEIGHT),
                        leg        = "topright",# Legend location. If NULL, none will be drawn
                        units      = .UNITS,    # Units to use in plotting
                        silent     = .SILENT){

  currFuncName <- getCurrFunc()

  retroDat     <- op[[scenario]]$outputs$retros
  numRetros    <- length(retroDat) + 1 # The +1 is to include the base model run
  ages         <- 0:(numRetros - 1)
  numYears     <- op[[scenario]]$inputs$data$nyr - op[[scenario]]$inputs$data$syr + 1
  years        <- op[[scenario]]$inputs$data$nyr:(op[[scenario]]$inputs$data$nyr - numRetros + 1)
  scenarioName <- op[[scenario]]$names$scenario

  # Set up a square matrix to hold the line cohort data
  # It will end up being an right lower triangular matrix
  # Rows are the ages 0-N for which an assessment was run in descending order, columns are the years in descending order
  recdevmat <- matrix(data = NA, nrow = numRetros, ncol = numRetros)

  # Get recruitment deviations for the main run, and a check to make sure it is the right length
  recdevs <- op[[scenario]]$outputs$par$log_rec_devs
  if(length(recdevs) != numYears){
    cat0(.PROJECT_NAME,"->",currFuncName,"The number of recdevs is not equal to the number of years. Turn projections off and re-run the model and retrospectives.")
    return(NULL)
  }

  # Fill in the last row of the matrix with the main run's output
  recdevmat[numRetros,] <- op[[scenario]]$outputs$par$log_rec_devs[numYears:(numYears - numRetros +1)]

  # Fill in the rest of the matrix with the retrospective data
  for(retro in 1:(numRetros-1)){
    numYears <- numYears - 1
    devvec <- op[[scenario]]$outputs$retros[[retro]]$outputs$par$log_rec_devs[numYears:(numYears - (numRetros - retro) + 1)]
    # Pad with NA's to make lower triangular
    devvec <- c(rep(NA,retro),devvec)
    recdevmat[numRetros-retro,] <- devvec
  }

  # plot each column as a line and label as the year
  figDir       <- op[[scenario]]$names$figDir
  filename     <- file.path(figDir,paste0(fileText,".png"))
  res          <- ps$pngres
  width        <- ps$pngw
  height       <- ps$pngh
  resScreen    <- ps$res
  widthScreen  <- ps$w
  heightScreen <- ps$h

  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)
  }else{
    windows(width=widthScreen,height=heightScreen)
  }

  for(year in length(years):1){
    # Remove the NAs so plotting starts at age 0
    recdevs <- recdevmat[,year][!is.na(recdevmat[,year])]
    agevec  <- ages[1:year]
    if(year == length(years)){
      xlabel <- paste0("Age in ",years[1])
      plot(agevec, recdevs, type="o", pch=20, lty=1, lwd=3, col=year,
           xlim=c(-1,max(ages)+1), ylim=ylim, ,ylab="Recruitment deviation", xlab=xlabel, axes=FALSE)
      abline(h=0, col="grey")
      text(agevec[length(agevec)] + 0.25, recdevs[length(recdevs)], years[year], col=year, cex=0.75)
      axis(1, at=ages, labels=ages)
      axis(2, at=ylim[1]:ylim[2], labels=ylim[1]:ylim[2], las=2)
      box()
    }else{
      lines(agevec, recdevs, type="o", pch=20, lty=1, lwd=3, col=year, ylim=ylim)
      text(agevec[length(agevec)] + 0.25, recdevs[length(recdevs)], years[year], col=year, cex=0.75)
    }
  }

  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}
