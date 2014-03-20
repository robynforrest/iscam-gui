#**********************************************************************************
# ccamRetro.r
# This file contains the code necessary to plot Retrospectives.
# 
# Assumes the opList has been built and has retrospective data in position 5,
# i.e. opList[[scenario]][[5]] contains a list of the retrospective outputs
#
# Author            : Chris Grandin
# Development Date  : December 2011 - January 2012
#
#**********************************************************************************

fig.retro <- function(whichPlot="biomass",ylimit=6,useMaxYlim=T,lty=1,lwd=2,pch=20){
  # plots Spawning stock biomiass, depletion, and recruitment retrospectives
  # whichPlot can be:
  # 1. "biomass"
  # 2. "depletion"
  # 3. "recruits"
  # Assumes that opList[[1]] is populated (i.e. there is at least one scenario)
  op	<- par(no.readonly=T)
  val <- getWinVal()
  scenario <- val$entryScenario

  baseRunName <- strsplit(opList[[scenario]][[1]],"/")[[1]][3] # Gets the run's folder name out of the reletive path
  runNames <- baseRunName
  baseRep <- opList[[scenario]][[4]]
  base <- 1
  color <- 1
  colors <- color
  # Base run is loaded, now plot it and add the retrospectives to the plot
  
  # *************************************** RECRUITMENT RETROSPECTIVE ******************************************  
  if(whichPlot == "recruits"){
    if(useMaxYlim){
      # get max of all retros first
      yUpperLimit <- max(baseRep$rt)
      for(retro in 1:length(opList[[scenario]][[5]])){
        yUpperLimit <- max(yUpperLimit,opList[[scenario]][[5]][[retro]]$rt)
      }
    }else{
      yUpperLimit <- ylimit
    }

    plot(baseRep$yr[1:(length(baseRep$yr)-1)],
         baseRep$rt,
         type="b",
         ylab="Age-1 recruits",
         ylim=c(0,yUpperLimit),
         xlab="Year",
         pch=pch,
         col=color,
         lwd=lwd,
         lty=lty)
    
    for(retro in 1:length(opList[[scenario]][[5]])){ # number of retrospectives
      color <- color + 1
      colors <- c(colors,color)
      lines(opList[[scenario]][[5]][[retro]]$yr[1:(length(opList[[scenario]][[5]][[retro]]$yr)-1)],
            opList[[scenario]][[5]][[retro]]$rt,
            lty=lty,
            col=color,
            lwd=lwd,
            ylim=c(0,yUpperLimit),
            type="b",
            xaxt="n")
      runNames <- c(runNames,paste(baseRunName," - ",retro))
    }
    legend("topright",runNames,lty=lty,col=colors,bty="n",lwd=lwd) 
    filename <- paste("Retrospective_Scenario",scenario,"_Recruits",sep="")
    saveFig(filename)

  # *************************************** BIOMASS RETROSPECTIVE ******************************************  
  }else if(whichPlot == "biomass"){
    if(useMaxYlim){
      yUpperLimit <- max(baseRep$sbt)
      for(retro in 1:length(opList[[scenario]][[5]])){
        yUpperLimit <- max(yUpperLimit,opList[[scenario]][[5]][[retro]]$sbt)
      }
    }else{
      yUpperLimit <- ylimit
    }
    plot(baseRep$yrs,
         baseRep$sbt,
         type="l",
         col=color,
         lty=lty,
         lwd=lwd,
         ylim=c(0,yUpperLimit),
         xlab="Year",
         ylab="Spawning biomass",
         main="Spawning biomass",
         las=1)
    
    points(baseRep$yrs[1]-0.8,
           baseRep$sbo,
           col=color,
           pch=pch,
           lwd=lwd)
    for(retro in 1:length(opList[[scenario]][[5]])){ # number of retrospectives
      color <- color + 1
      colors <- c(colors,color)
      lines(opList[[scenario]][[5]][[retro]]$yrs,
            opList[[scenario]][[5]][[retro]]$sbt,
            type="l",
            col=color,
            lty=lty,
            lwd=lwd,
            ylim=c(0,yUpperLimit),
            xlab="",
            ylab="",
            las=1,
            xaxt="n")
      points(opList[[scenario]][[5]][[retro]]$yrs[1]-0.8,
             opList[[scenario]][[5]][[retro]]$sbo,
             col=color,
             pch=pch,
             lwd=lwd)
      runNames <- c(runNames,paste(baseRunName," - ",retro))
    }
    legend("topright",runNames,lty=lty,col=colors,bty="n", lwd=lwd) 
    filename <- paste("Retrospective_Scenario",scenario,"_Biomass",sep="")
    saveFig(filename)
    
  # *************************************** DEPLETION RETROSPECTIVE ******************************************  
  }else if(whichPlot == "depletion"){
    if(useMaxYlim){
      yUpperLimit <- max(baseRep$sbt/baseRep$sbo)
      for(retro in 1:length(opList[[scenario]][[5]])){
        yUpperLimit <- max(yUpperLimit,opList[[scenario]][[5]][[retro]]$sbt/opList[[scenario]][[5]][[retro]]$sbo)
      }
    }else{
      yUpperLimit <- ylimit
    }
    plot(baseRep$yrs,
         baseRep$sbt/baseRep$sbo,
         type="l",
         col=color,
         lty=lty,
         lwd=lwd,
         ylim=c(0,yUpperLimit),
         xlab="Year",
         ylab="Spawning Depletion",
         main="Spawning Depletion",
         las=1)
    abline(h=0.40, lwd=mtLineWidth, lty=mtLineType, col=mtLineColor)     
    for(retro in 1:length(opList[[scenario]][[5]])){ # number of retrospectives
      color <- color + 1
      colors <- c(colors,color)
      lines(opList[[scenario]][[5]][[retro]]$yrs,
            opList[[scenario]][[5]][[retro]]$sbt/opList[[scenario]][[5]][[retro]]$sbo,
            type="l",
            col=color,
            lty=lty,
            lwd=lwd,
            ylim=c(0,yUpperLimit),
            xlab="",
            ylab="",
            las=1,
            xaxt="n")          
      runNames <- c(runNames,paste(baseRunName," - ",retro))
    }
    lty <- c(rep(1,(length(runNames))),mtLineType)
    lwd <- c(rep(2,(length(runNames))),mtLineWidth)
    colors <- c(colors,mtLineColor)
    legend("topright",c(runNames,"Management target"),lty=lty,col=colors,bty="n", lwd=lwd) 
    filename <- paste("Retrospective_Scenario",scenario,"_Depletion",sep="")
    saveFig(filename)
  }
	par(op)
}
