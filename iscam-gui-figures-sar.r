#**********************************************************************************
# iscam-gui-figures-sar.r
# This file contains the code for plots used in the SAR report only
#
# Author            : Chris Grandin
# Development Date  : June 2015 - Present
#**********************************************************************************

# Plot catch, exploitation rate and median spawning biomass on one plot
plotce <- function(leg              = "topleft",
                   showtitle        = TRUE,
                   col              = 1,
                   from             = 1996,       # Year to plot from
                   to               = 2014,       # Year to plot to
                   scalefactor      = 1000,       # Divide the catch and discard by this factor
                   spaceBetweenBars = 0.5,        # space between each year's set of bars
                   opacity          = 50,         # The transparency of the bars
                   add              = FALSE,      # If TRUE, par will not be restored on exit
                   burnthin         = c(1000,1),
                   ci               = 95
                   ){
  # Catch plot for iscam model, plots by gear for landings and discards, split side-by-side bars
  currFuncName <- getCurrFunc()
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

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

  dcat <- dcat[dcat$year %in% from:to,]
  dcat$catch <- dcat$catch / scalefactor

  # Make a matrix out of the two data series, and plot side-by-side
  allCatch <- as.matrix(cbind(jcat[,2],dcat[,2]))

  col1        <- .getShade(1, opacity)
  col2        <- .getShade(2, opacity)

  #par(mar=c(5.1,4.1,4.1,2.1)) # This is the default
  par(mar=c(5.1,4.1,4.1,4.1))

  windows(10, 7)

  b <- barplot(t(allCatch),
               #inset=c(-0.25,0),
               axes=FALSE,
               col=c(col1,col2),
               border=c("black","black"),
               ylim=c(0,1.1*max(apply(allCatch, 1, sum))),
               las=2)

  axis(1,
       at = b,
       labels = years)
  axis(2,
       at = seq(0,20000,5000),
       labels = seq(0,2,0.5))

  burn <- burnthin[[1]]
  thin <- burnthin[[2]]
  # Add exploitation rate line
  ut <- op[[1]]$outputs$mcmc$ut[[1]][[1]]
  utw <- window(mcmc(ut), start=burn, thin=thin)
  quants <- getQuants(utw, ci)
  umed <- quants[2,]
  par(new=TRUE)
  plot(b, umed, type="l", lwd=3, lty=2, col="blue", axes=FALSE, ylim=c(0,1), ylab="", xlab="")
  # Add relative spawning biomass line
  sbt <- window(mcmc(op[[1]]$outputs$mcmc$sbt[[1]]), start=burn, thin=thin)
  bo <- as.vector(window(mcmc(op[[1]]$outputs$mcmc$params$bo), start=burn, thin=thin))
  depl <- sbt / bo
  quants <- getQuants(depl, ci)
  depmed <- quants[2,]
  depmed <- depmed[-length(depmed)]
  par(new=TRUE)
  plot(b, depmed, type="b", pch=19, lwd=2, lty=2, col="black", ylim=c(0,1), axes=FALSE, ylab="", xlab="")

  cex <- 0.7
  axis(4,
       at = seq(0,1,0.2),
       labels = seq(0,1,0.2))
  box()
  xlabel <- "Year"
  ylabel <- "Catch (x 10,000 t)"
  ylabel2 <- "Proportion"

  mtext(side=1,line=2,xlabel)
  mtext(side=2,line=2,ylabel)
  mtext(side=4,line=2,ylabel2)

  if(!is.null(leg)){
    legendList       <- c("Landings","At-sea releases","Female exploitation rate","Relative female spawning biomass")
    legendShadeCols  <- c(c(col1,col2),c("blue","black"))
    legendBorderCols <- c(c("black","black"),0,0)
    legend(leg,legendList,col=legendShadeCols, pch=c(15,15,NA,19), pt.cex=1, lty=c(0,0,2,2), lwd=c(0,0,2,2), bty="n", merge=TRUE)
    #legend(leg,legendList,col=legendBorderCols,fill=legendShadeCols,bty="n", merge=TRUE)
  }
}

# Plot the DFO precautionary figure. This currently only works for one scenario
plotprecaut <- function(opacity  = 50,         # The transparency of the boxes
                        add      = FALSE,      # If TRUE, par will not be restored on exit
                        burnthin = c(1000,1),
                        props    = c(0.05, 0.25, 0.5, 0.75, 0.975),
                        plotbmsy = FALSE       # If TRUE, Bt/Bmsy will be plotted. If FALSE, Bt/B0 will be plotted
                        ){
  currFuncName <- getCurrFunc()
  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }
  burn <- burnthin[[1]]
  thin <- burnthin[[2]]

  sb <- window(mcmc(op[[1]]$outputs$mcmc$sbt[[1]]), start=burn, thin=thin)
  sbt <- sb[,ncol(sb)]
  # Get final year from names of data columns
  endyear <- colnames(sb)[length(colnames(sb))]

  bmsy <- window(mcmc(op[[1]]$outputs$mcmc$params$bmsy), start=burn, thin=thin)
  bo <- window(mcmc(op[[1]]$outputs$mcmc$params$bo), start=burn, thin=thin)
  btbmsy <- sbt/bmsy
  btbo <- sbt/bo
  if(plotbmsy){
    quants <- quantile(btbmsy, props)
  }else{
    quants <- quantile(btbo, props)
  }
  windows(8, 3.5)
  shade <- .getShade("blue", opacity)

  # Add 0.4Bmsy and 0.8Bmsy lines and text for critical, cautious, and healthy zones
  if(plotbmsy){
    boxplot(quants, horizontal=TRUE, ylim=c(0,max(quants)), col=shade, border="blue", lty=1, lwd=1.5, axes=FALSE,
            xlab=bquote("B"[.(endyear)] ~ "/" ~ "B"[msy]))
    abline(v=0.4, col="red", lty=2, lwd=2)
    abline(v=0.8, col="green3", lty=2, lwd=2)
    #axis(3, at=c(0.4,0.8), col.ticks=c("red","orange"))
    axis(1, at=c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5))
    text(0.1,1,"Critical",srt=0,cex=1.5,col="red",font=2, srt=90)
    text(0.6,1,"Cautious",srt=0,cex=1.5,col="orange",font=2, srt=90)
    text(1.1,1,"Healthy",srt=0,cex=1.5,col="green3",font=2, srt=90)
  }else{
    boxplot(quants, horizontal=TRUE, ylim=c(0,max(quants)), col=shade, border="blue", lty=1, lwd=1.5, axes=FALSE,
            xlab=bquote("B"[.(endyear)] ~ "/" ~ "B"[0]))
    # Add 0.2B0 and 0.4B0 lines and text for critical, cautious, and healthy zones
    abline(v=0.2, col="red", lty=2, lwd=2)
    abline(v=0.4, col="green3", lty=2, lwd=2)
    #axis(3, at=c(0.2,0.4), col.ticks=c("red","orange"))
    axis(1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2))
  }
  box()
}
