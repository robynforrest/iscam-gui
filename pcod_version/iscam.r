#**********************************************************************************
# iscam.r
# This file contains the code for a front end GUI controller for iscam using
# Tcl/Tk windows implemented using the R package 'PBSModelling'.  The data
# structure used is an opList, which is a list of lists, see loadScenarios.r for
# details on this structure. This file assumes that an object called 'opList'
# exists and is a valid opList.
#
# Author            : Chris Grandin/Robyn Forrest
# Development Date  : December 2011 - Present
#
# Source this file, then call iscam() with whatever arguments you need.
#
# iscam(reload=F,silent=F,copyADMBExecutables=F)
#
# To change/create global variables, find the function assignGlobals() in this file
# 
#**********************************************************************************
removeAllExcept <- function(vars="opList", envir = .GlobalEnv) {
  # removeAllExcept()
  # removes everything in the workspace except for what's in the vars list
  # Upon finishing, the workspace will contain whatever is in the vars list
  # plus the objects 'removeAllExcept' (this function) and 'modelLoaded'
  # which tells the software that the model has already been loaded.
  # - vars - A list of objects to keep, typically just 'opList'
  # - envir - environment to clear, typically .GlobalEnv
  vars <- c(vars, "removeAllExcept")
  keep <- match(x = vars, table = ls(all=T,envir = envir))
  if(any(is.na(keep))){
    assign("modelLoaded",F,envir=.GlobalEnv)
  }else{
    rm(list=ls(all=T,envir=envir)[-keep],envir=envir)
    assign("modelLoaded",T,envir=.GlobalEnv)
  }
}
removeAllExcept()

require(Hmisc)
require(MASS)
require(KernSmooth)
require(MCMCpack)
require(coda)
require(PBSmodelling)
require(grDevices)

options(stringsAsFactors=FALSE)
# make warnings print out when they occur instead of at the end
options(warn=1)

# Runtime stats constants for plotting
.MAXGRAD <- 0.0001
.FUNEVALS <- 150
.PCHCODE <- 16

source("reptolist.r")
source("iscamFriedEgg.r")
source("iscamExecutiveSummary.r")
source("iscamFigs.r")
source("iscamTables.r")
source("iscamSens.r")
source("iscamRetro.r")
source("iscamADMBFileControl.r")
source("iscamLoadScenarios.r")
source("iscamUtils.r")

getShade <- function(color,opacity){
  # getShade()
  # returns an rgb string of the specified color and opacity:
  # - color - an R color either text or decimal number
  # - opacity - 2-decimal-digit string (00-99), i.e. "20" means 20%
  # Notes: format of returned string is #RRGGBBAA
  #        where RR=red, a 2-hexadecimal-digit string
  #        GG=green, a 2-hexadecimal-digit string
  #        BB=blue, a 2-hexadecimal-digit string
  #        AA=alpha or opacity
  
  colorDEC <- col2rgb(color)
  colorHEX <- sprintf("%X", colorDEC)
  for(i in 1:length(colorHEX)){
    if(nchar(colorHEX[i])==1){
      colorHEX[i] <- paste("0",colorHEX[i],sep="")
    }
  }
  shade <- paste("#",colorHEX[1],colorHEX[2],colorHEX[3],opacity,sep="")
  return(shade)
}

gletter<-function(letter){
  # gletter()
  # adds letters to plot panels
  # - letter - the letter to place on the panel
  usr <- par("usr")
  inset.x <- 0.05*(usr[2]-usr[1])
  inset.y <- 0.05*(usr[4]-usr[3])
  text(usr[1]+inset.x,usr[4]-inset.y,paste("(",letters[letter],")",sep=""),cex=1.,font=1)
}

assignGlobals <- function(scenario=1,silent=F){
  # assignGlobals()
  # assigns global variables used in the model, directory names,  and the data object 'A'
  # used for plotting.
  # - scenario - which scenario to set up
  # - silent T/F - show messages on command line
  
  # A is the pcod_iscam model output object
   assign("A",opList[[scenario]][[4]],envir=.GlobalEnv)
  assign("figDir",opList[[scenario]][[2]],envir=.GlobalEnv)
  assign("tabDir",opList[[scenario]][[3]],envir=.GlobalEnv)
  # saveon is a toggle for writing figures to disk
  assign("saveon",F,envir=.GlobalEnv)
  # model variables - ideally these are read in not hardwired like this
  assign("age",opList[[scenario]][[4]]$age,envir=.GlobalEnv)
  assign("nage",age[length(age)],envir=.GlobalEnv)
  assign("yr",opList[[scenario]][[4]]$yr,envir=.GlobalEnv)
  assign("yrs",opList[[scenario]][[4]]$yrs,envir=.GlobalEnv)
  assign("nyrs",length(yrs),envir=.GlobalEnv)
   assign("nyear",length(yr),envir=.GlobalEnv)
  assign("nyr",yr[length(yr)],envir=.GlobalEnv)
  assign("ngear",opList[[scenario]][[4]]$ngear,envir=.GlobalEnv)
   assign("delaydiff",opList[[scenario]][[4]]$delaydiff,envir=.GlobalEnv)
 # catch streams based on estimated parameters
 # assign(".FMSYFORCFLAG",-999,envir=.GlobalEnv)  # Needed by admb for catch streams (es.table.h)
#  assign(".F40FORCFLAG",-888,envir=.GlobalEnv)  # based on estimated values
#  assign(".SSFORCFLAG",-777,envir=.GlobalEnv)  # SS ABC catch stream
  # Set age comp data
  
  #@@@@@@@@@@@@@@ RF JULY 2012 additions @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  #RF made this change so that the code doesn't try and read in age observations that aren't there in the ageless model (cntrl 14 == 3)
  assign("AgeLikelihood",opList[[scenario]][[4]]$cntrl[14],envir=.GlobalEnv)

  if(AgeLikelihood!=3){  #do not do this if model is 'ageless'  3 is the only survey with age obs right now
	 # assign("Acom_obs",opList[[scenario]][[4]]$A[which(opList[[scenario]][[4]]$A[,2]==1),],envir=.GlobalEnv)
	  assign("Asurv_obs",opList[[scenario]][[4]]$A,envir=.GlobalEnv)  #[which(opList[[scenario]][[4]]$A[,2]==3),]
	 # assign("Acom_est",opList[[scenario]][[4]]$Ahat[which(opList[[scenario]][[4]]$Ahat[,2]==1),],envir=.GlobalEnv)
	  assign("Asurv_est",opList[[scenario]][[4]]$Ahat,envir=.GlobalEnv)
	  #assign("Acom_res",opList[[scenario]][[4]]$A_nu[which(opList[[scenario]][[4]]$A_nu[,2]==1),],envir=.GlobalEnv)
	  assign("Asurv_res",opList[[scenario]][[4]]$A_nu,envir=.GlobalEnv)
  }
   assign("nits",opList[[scenario]][[4]]$nits,envir=.GlobalEnv) # num survey indices

  # Set plot output type
  assign("plotType","png",envir=.GlobalEnv)
  # Set plot globals
  # mt variables control the management target line
  assign("mtLineColor","green",envir=.GlobalEnv)
  assign("mtLineType",3,envir=.GlobalEnv)  
  assign("mtLineWidth",3,envir=.GlobalEnv)  
  assign("mpdLineColor","blue",envir=.GlobalEnv)
  .setBurnThin(silent=silent)
}

loadData <- function(reloadScenarios=T,copyADMBExecutables=F,silent=F){
  # loadData()
  # loads all model output data from all scenarios.
  # - reloadScenarios T/F - reload the data from all model output files in all scenarios.
  # - copyADMBExecutables T/T copy the admb executable from admb folder to each scenario folder.
  #   this is used for the case where you want to run the model from inside its scenario folder
  #   which makes it possible to run multiple models at one time on multi-processor machines.
  # - silent T/F - show messages on command line

  if(!exists("modelLoaded",envir=.GlobalEnv)){
    modelLoaded <<- F
  }
  if(reloadScenarios || !modelLoaded){
    loadScenarios(silent=silent)
    modelLoaded <<- T
    if(!silent){
      cat("loadData: Loading data from model output files.\n")
    }
  }else{
    if(!silent){
      cat("loadData: Warning! Using previously loaded data for GUI.\n\n")
    }
  }
  if(copyADMBExecutables){
    copyExecutableToScenariosFolders(silent=silent)
  }
}

pcod_iscam <- function(reloadScenarios=F,copyADMBExecutables=T,silent=F){
  # pcod_iscam()
  # launches the CCAM GUI.
  # - reloadScenarios T/F - reload the data from all model output files in all scenarios.
  # - copyADMBExecutables T/F copy the admb executable from admb folder to each scenario folder.
  # - silent T/F - show messages on command line
  
  graphics.off()  # Destroy graphics window if it exists
  loadData(reloadScenarios=reloadScenarios,
           copyADMBExecutables=copyADMBExecutables,
           silent=silent)
  return(.pcod_iscamGUIsetup("pcod_iscamGui",silent=silent))
}

.loadPlottingLimits <- function(){
  # load the plotting limits into proper entry and checkboxes
  # BIOMASS YLIMITS
  winList <- NULL
  val <- getWinVal()
  scenario <- val$entryScenario
  if(!is.null(opList[[scenario]][[4]]$biomassYlim)){
    winList <- c(winList,biomassYlim=opList[[scenario]][[4]]$biomassYlim)
    winList <- c(winList,biomassSensYlim=opList[[scenario]][[4]]$biomassYlim)
    winList <- c(winList,biomassRetroYlim=opList[[scenario]][[4]]$biomassYlim)
  }
  if(!is.null(opList[[scenario]][[4]]$maxBiomassYlim)){
    winList <- c(winList,maxBiomassYlim=opList[[scenario]][[4]]$maxBiomassYlim)
    winList <- c(winList,maxBiomassSensYlim=opList[[scenario]][[4]]$maxBiomassYlim)
    winList <- c(winList,maxBiomassRetroYlim=opList[[scenario]][[4]]$maxBiomassYlim)
  }
  #TOTAL BIOMASS LIMITS
   if(!is.null(opList[[scenario]][[4]]$tbiomassYlim)){
      winList <- c(winList,tbiomassYlim=opList[[scenario]][[4]]$tbiomassYlim)
      winList <- c(winList,biomassSensYlim=opList[[scenario]][[4]]$tbiomassYlim)
      winList <- c(winList,biomassRetroYlim=opList[[scenario]][[4]]$tbiomassYlim)
    }
    if(!is.null(opList[[scenario]][[4]]$maxBiomassYlim)){
      winList <- c(winList,maxtBiomassYlim=opList[[scenario]][[4]]$maxtBiomassYlim)
      winList <- c(winList,maxtBiomassSensYlim=opList[[scenario]][[4]]$maxtBiomassYlim)
      winList <- c(winList,maxtBiomassRetroYlim=opList[[scenario]][[4]]$maxtBiomassYlim)
  }  
  # DEPLETION YLIMITS
  if(!is.null(opList[[scenario]][[4]]$depletionYlim)){
    winList <- c(winList,depletionYlim=opList[[scenario]][[4]]$depletionYlim)
    winList <- c(winList,depletionSensYlim=opList[[scenario]][[4]]$depletionYlim)
    winList <- c(winList,depletionRetroYlim=opList[[scenario]][[4]]$depletionYlim)
  }
  if(!is.null(opList[[scenario]][[4]]$maxDepletionYlim)){
    winList <- c(winList,maxDepletionYlim=opList[[scenario]][[4]]$maxDepletionYlim)
    winList <- c(winList,maxDepletionSensYlim=opList[[scenario]][[4]]$maxDepletionYlim)
    winList <- c(winList,maxDepletionRetroYlim=opList[[scenario]][[4]]$maxDepletionYlim)
  }
  # RECRUITMENT YLIMITS
  if(!is.null(opList[[scenario]][[4]]$recruitmentYlim)){
    winList <- c(winList,recruitmentYlim=opList[[scenario]][[4]]$recruitmentYlim)
    winList <- c(winList,recruitmentSensYlim=opList[[scenario]][[4]]$recruitmentYlim)
    winList <- c(winList,recruitmentRetroYlim=opList[[scenario]][[4]]$recruitmentYlim)
  }
  if(!is.null(opList[[scenario]][[4]]$maxRecruitmentYlim)){
    winList <- c(winList,maxRecruitmentYlim=opList[[scenario]][[4]]$maxRecruitmentYlim)
    winList <- c(winList,maxRecruitmentSensYlim=opList[[scenario]][[4]]$maxRecruitmentYlim)
    winList <- c(winList,maxRecruitmentRetroYlim=opList[[scenario]][[4]]$maxRecruitmentYlim)
  }
  try(setWinVal(winList), silent=silent)
}

.pcod_iscamGUIsetup <- function(win,silent=F){
  if(win=="pcod_iscamGui"){
    viewHeader <- data.frame()
    viewSensitivityGroups <- data.frame()
    for(scenario in 1:length(opList)){
      viewHeader            <- rbind(viewHeader,opList[[scenario]][[1]])
      viewSensitivityGroups <- rbind(viewSensitivityGroups,opList[[scenario]][[4]]$SensitivityGroup)
    }
    colnames(viewHeader) <- "Scenario List"
    colnames(viewSensitivityGroups) <- "Sensitivity Group"

    scenarioHeader <- cbind(viewHeader,viewSensitivityGroups)
    
    assign("viewHeader", viewHeader,envir=.GlobalEnv)
    assign("viewSensitivityGroups", viewSensitivityGroups,envir=.GlobalEnv)
    assign("scenarioHeader", scenarioHeader,envir=.GlobalEnv)

    assign("scenarioList",as.numeric(rownames(viewHeader)),pos=1)
    assign("A",opList[[1]][[4]],envir=.GlobalEnv)  # the 4th member in each model list is the data
    
    createWin(paste(getwd(),"/",win,"Win.txt",sep=""),env=.GlobalEnv)
    
    winList <- c(entryScenario=1)
    try(setWinVal(winList), silent=silent)

    .loadPlottingLimits()
    
    # Set Base as default start model, assumes Base 
    assignGlobals(1)
  }
}

.checkEntries <- function(){
  # Ensures that the entry in the Scenarios box on the GUI is within proper limits
  # Issues an alert box if they are not, and returns FALSE
  # If they are within limits, returns TRUE
  val <- getWinVal()
  scenarioList <- as.numeric(rownames(viewHeader))
  currScenario <- val$entryScenario
  if(currScenario<min(scenarioList) | currScenario>max(scenarioList)){
    showAlert(paste("Your scenario must be between ",
                    min(scenarioList)," and ",
                    max(scenarioList),".\nNo plot will be drawn.",sep=""),
              title="Scenario Error",icon="warning")
    return(FALSE)
  }
  return(TRUE)
}

.setBurnThin <- function(silent=F){
  val <- getWinVal()
  assign("Burn",val$burn,envir=.GlobalEnv)
  assign("Thin",val$thin,envir=.GlobalEnv)
  assign("Nbin",val$nbin,envir=.GlobalEnv)
}

.writeAllPlots <- function(silent=F){
  # write all figures for all scenarios to disk
  scenarioList <- as.numeric(rownames(viewHeader))
  for(scenario in scenarioList){
    assignGlobals(scenario)
    .writePlots(silent=silent)
  }  
}

.writeAllTables <- function(silent=F){
  # write all tables for all scenarios to disk
  scenarioList <- as.numeric(rownames(viewHeader))
  for(scenario in scenarioList){
    assignGlobals(scenario)
    .writeTables(silent=silent)
  }  
}

.writeTables <- function(silent=F){
  #write all tables for the given scenario to disk
  .setBurnThin(silent=silent)

  cat("\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
  #cat("No tables currently implemented\n")
  cat("Writing tables\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
 if(delaydiff && nyr>=2012){
   try(table.projections(), silent=silent)
  try(table.decision(),silent=silent)
   # Make Fishing mortality Quantiles and MPD table
   quantProbs <- c(0.025,0.5,0.975)
   weightScale <- 1000.0
   table.mcmc.mpd(mcmcData  = A$mc.ft,
                  burnin    = Burn,
                  probs     = quantProbs,
                  mpdData   = A$ft,
                  colLabels = A$yr,
                  roundDec  = 4, # Number of decimal places
                  tableName = "FishingMortalityQuants")
   # Make Biomass Quantiles and MPD table
   table.mcmc.mpd(mcmcData  = A$mc.sbt / weightScale,
                  burnin    = Burn,
                  probs     = quantProbs,
                  mpdData   = A$sbt / weightScale,
                  colLabels = A$yrs,
                  formatOut = "%1.3f",
                  roundDec  = 3, # Number of decimal places
                  tableName = "BiomassQuants")
   # Make Recruitment Quantiles and MPD table
   table.mcmc.mpd(mcmcData  = A$mc.rt,
                  burnin    = Burn,
                  probs     = quantProbs,
                  mpdData   = A$rt,
                  colLabels = A$yr[-(1:2)], # The 2 is because there is age-2 recruitment
                  roundDec  = 4, # Number of decimal places
                  tableName = "RecruitmentQuants")
   # Make Paramter Quantiles and MPD values table
   mcmcParamTable   <- cbind(exp(A$mc$log.ro), A$mc$h, exp(A$mc$log.m), exp(A$mc$log.rbar), exp(A$mc$log.rinit), A$mc$bo)
   paramNames       <- c("r0","steepness","m","rbar","rbar_init","b0")
   mpdParamVector   <- c(A$ro, A$steepness, A$m, A$rbar, A$rinit, A$sbo)
   # Add variable number of q's
   for(qInd in 1:length(A$q)){
     mcmcParamTable <- cbind(mcmcParamTable, eval(parse(text=paste0("A$mc$q",qInd))))
     mpdParamVector <- c(mpdParamVector, A$q[qInd])
     paramNames     <- c(paramNames, paste0("q",qInd))
   }
   colnames(mcmcParamTable) <- paramNames
   table.mcmc.mpd(mcmcData  = mcmcParamTable,
                  burnin    = Burn,
                  probs     = quantProbs,
                  mpdData   = mpdParamVector,
                  colLabels = paramNames,
                  roundDec  = 4, # Number of decimal places
                  tableName = "ParameterQuants")

 }else{
   cat("No decision tables for age-structured model or bridging analysis\n")
 }

 # try(mb.table.all.param.est(roundDec=2,formatOut="%1.2f"), silent=silent)
 # try(table.i(), silent=silent)
 # try(table.h(mle=F,tableType="ssb"), silent=silent)
 # try(table.h(mle=F,tableType="depletion"), silent=silent)
 # try(table.h(mle=F,tableType="f40spr"), silent=silent)
 
  
  #try(table.b(), silent=silent)
  #try(table.c(), silent=silent)
  #try(table.d(), silent=silent)
  #try(table.e1(), silent=silent)
  #try(table.e2(), silent=silent)
  #try(table.f(), silent=silent)
  #try(table.h(mle=T,tableType="ssb"), silent=silent)
  #try(table.h(mle=T,tableType="depletion"), silent=silent)
  #try(table.h(mle=T,tableType="f40spr"), silent=silent)
  #try(table.1(), silent=silent)
  #try(table.2(), silent=silent)
  #try(table.3(), silent=silent)
  #try(table.4(), silent=silent)  
}

.writeRetroPlots <- function(silent=F){
  assign("saveon",T,envir=.GlobalEnv)
  val <- getWinVal()
  try(fig.retro(whichPlot="biomass",
            ylimit=val$biomassYlim,
            useMaxYlim=val$maxBiomassYlim), silent=silent)
  try(fig.retro(whichPlot="depletion",
            ylimit=val$depletionYlim,
            useMaxYlim=val$maxDepletionYlim), silent=silent)
  try(fig.retro(whichPlot="recruits",
                ylimit=val$recruitmentYlim,
                useMaxYlim=val$maxRecruitmentYlim), silent=silent)
  assign("saveon",F,envir=.GlobalEnv)
}

.writeSensPlots <- function(silent=F){
  # write overlay sensitivity plots
  assign("saveon",T,envir=.GlobalEnv)
  val <- getWinVal()
  uniqueSensitivityGroups <- c()  # base must be 0
  for(scenario in 1:length(opList)){
    # count number of unique sensitivity groups
    if(!is.element(opList[[scenario]][[4]]$SensitivityGroup,uniqueSensitivityGroups) && opList[[scenario]][[4]]$SensitivityGroup != 0){
        uniqueSensitivityGroups <- c(uniqueSensitivityGroups,opList[[scenario]][[4]]$SensitivityGroup)
    }
  }
  for(sensitivityGroup in uniqueSensitivityGroups){
    try(fig.base.vs.sens(sensitivityGroup=sensitivityGroup,
                         whichPlot="biomass",
                         ylimit=val$biomassYlim,
                         useMaxYlim=val$maxBiomassYlim),silent=silent)
    try(fig.base.vs.sens(sensitivityGroup=sensitivityGroup,
                         whichPlot="depletion",
                         ylimit=val$depletionYlim,
                         useMaxYlim=val$maxDepletionYlim),silent=silent)
    try(fig.base.vs.sens(sensitivityGroup=sensitivityGroup,
                         whichPlot="recruits",
                         ylimit=val$recruitmentYlim,
                         useMaxYlim=val$maxRecruitmentYlim),silent=silent)
  }
  assign("saveon",F,envir=.GlobalEnv)
}

.writePlots <- function(silent=F){
  # write all figures for the given scenario to disk
  .setBurnThin()
  val <- getWinVal()
  assign("saveon",T,envir=.GlobalEnv)
  # Landings
  try(fig.a(),silent=silent)
   # Effort
  try(fig.effort(),silent=silent)
  # Biomass MCMC with MPD overlay
  try(fig.b(includeMPD=T,ylimit=val$biomassYlim,useMaxYlim=val$maxBiomassYlim),silent=silent)
  # Depletion MCMC with MPD overlay 
  try(fig.c(includeMPD=T,ylimit=val$depletionYlim,useMaxYlim=val$maxDepletionYlim),silent=silent)
  # Recruitment MCMC
  try(fig.dmcmc(ylimit=val$recruitmentYlim,useMaxYlim=val$maxRecruitmentYlim),silent=silent)
   # Recruitment MPD
  try(fig.d(ylimit=val$recruitmentYlim,useMaxYlim=val$maxRecruitmentYlim),silent=silent)
  # Biomass and recruitment, two-panel plot
  #try(fig.biomass.recruits(yBiomassYlim=val$biomassYlim,
  #                         useMaxBiomassYlim=val$maxBiomassYlim,
  #                         yRecruitmentYlim=val$recruitmentYlim,
  #                         useMaxRecruitmentYlim=val$maxRecruitmentYlim), silent=silent)

  try(fig.e1(),silent=silent)
  try(fig.e2(),silent=silent)
  try(fig.g(),silent=silent)
  try(fig.h(),silent=silent)
  try(fig.i(),silent=silent)
  try(fig.j(),silent=silent)
  try(fig.comm.age.residuals1(),silent=silent)
  try(fig.comm.age.residuals2(),silent=silent)
  try(fig.comm.age.props(),silent=silent)
  try(fig.survey.age.residuals1(),silent=silent)
  try(fig.survey.age.residuals2(),silent=silent)
  try(fig.survey.age.props(),silent=silent)
  try(fig.comm.age.props.fit(),silent=silent)
  try(fig.survey.age.props.fit(),silent=silent)
  try(fig.surveybiomass.fit(),silent=silent)
  try(fig.selectivity(),silent=F)
  try(fig.Fmcmc(),silent=F)
  try(fig.phase(),silent=F)
  try(fig.time.varying.selectivity(2),silent=F)
  try(fig.time.varying.selectivity(1),silent=F)
  try(fig.weightFit(),silent=silent)
   try(fig.catchFit(),silent=silent)
  try(fig.mcmc.priors.vs.posts(exFactor=1.0,showEntirePrior=T),silent=silent)
   try(fig.mcmc.priors.vs.posts2(exFactor=1.0),silent=silent)
   try(fig.mcmc.priors.vs.postskey(exFactor=1.0),silent=silent)
  try(fig.mcmc.trace(),silent=silent)
  try(fig.RecAnomMCMC(), silent=silent)
  #try(fig.mcmc.density(), silent=silent)
  #try(fig.mcmc.autocor(),silent=silent)
  #try(fig.mcmc.geweke(), silent=silent)

  try(fig.estimated.params.pairs(), silent=silent)
  try(fig.estimated.params.pairs2(), silent=silent)
  try(fig.estimated.params.pairs.key(), silent=silent)
   try(fig.estimated.params.pairs.no.log.key(), silent=silent)
  try(fig.variance.partitions(), silent=silent)
  if(delaydiff==1){
	 if(nyr>=2012) {      
	      try(fig.Allcontrol.pts.Box(), silent=silent)
	      try(fig.MSYcontrol.pts(), silent=silent)
	      try(fig.Histcontrol.pts(), silent=silent)
	      try(fig.Benchmarks(), silent=silent) }
  }
  assign("saveon",F,envir=.GlobalEnv)
}

.doPlots <- function(){
  # val is the value object from GetWinVal()
  val <- getWinVal()
  pType <- val$viewPlotType
  .setBurnThin()
  oldpar <- par( no.readonly=TRUE )
  if(.checkEntries()){
    #################################################    
    # Call figure code from pcod_iscamExecutiveSummary.r #
    #################################################
    if(pType=="sLandings"){
      fig.a()
    }else if(pType=="sEffort"){
       fig.effort()
   }else if(pType=="sBiomass"){
      fig.b(includeMPD=F,
            ylimit=val$biomassYlim,
            useMaxYlim=val$maxBiomassYlim,
            opacity=20,
            main="Spawning biomass",
            xlab="Year")
    }else if(pType=="sBiomassMPDOver"){
      fig.b(includeMPD=T,
            ylimit=val$biomassYlim,
            useMaxYlim=val$maxBiomassYlim,
            opacity=20,
            main="Spawning biomass",
            xlab="Year")
    }else if(pType=="sBiomassMPD"){
      fig.biomass.mpd(ylimit=val$biomassYlim,
                      useMaxYlim=val$maxBiomassYlim,
                      main="Spawning biomass",
                      xlab="Year")
    }else if(pType=="tBiomass"){
          fig.bt(includeMPD=F,
                ylimit=val$tbiomassYlim,
                useMaxYlim=val$maxtBiomassYlim,
                opacity=20,
                main="Total biomass",
                xlab="Year")
        }else if(pType=="tBiomassMPDOver"){
          fig.bt(includeMPD=T,
                ylimit=val$tbiomassYlim,
                useMaxYlim=val$maxtBiomassYlim,
                opacity=20,
                main="Total biomass",
                xlab="Year")
        }else if(pType=="tBiomassMPD"){
          fig.bt.mpd(ylimit=val$tbiomassYlim,
                          useMaxYlim=val$maxtBiomassYlim,
                          main="Total biomass",
                          xlab="Year")
    } else if(pType=="sBiomassRecruits"){
      fig.biomass.recruits(yBiomassYlim=val$biomassYlim,
                           useMaxBiomassYlim=val$maxBiomassYlim,
                           yRecruitmentYlim=val$recruitmentYlim,
                           useMaxRecruitmentYlim=val$maxRecruitmentYlim)

    }else if(pType=="sDepletion"){
      fig.c(includeMPD=F,
            ylimit=val$depletionYlim,
            useMaxYlim=val$maxDepletionYlim,
            xlab="Year",
            main="Spawning depletion")
    }else if(pType=="sDepletionMPDOver"){
      fig.c(includeMPD=T,
            ylimit=val$depletionYlim,
            useMaxYlim=val$maxDepletionYlim,
            xlab="Year",
            main="Spawning depletion")
    }else if(pType=="sDepletionMPD"){
      fig.depletion.mpd(ylimit=val$depletionYlim,useMaxYlim=val$maxDepletionYlim)

    }else if(pType=="sRecruits"){
      fig.dmcmc(ylimit=val$recruitmentYlim,
            useMaxYlim=val$maxRecruitmentYlim,
            xlab="Year",
            main="Recruitment")
     }else if(pType=="sRecruitsMPD"){
	  fig.d(ylimit=val$recruitmentYlim,
		useMaxYlim=val$maxRecruitmentYlim,
		xlab="Year",
    main="Recruitment")
    }else if(pType=="sSPRMSY"){
      fig.e1()
    }else if(pType=="sSPRf40"){
      fig.e2()
    }else if(pType=="sFMCMC"){
      fig.Fmcmc()
     }else if(pType=="sFMPD"){
      fig.Fmpd()
    }else if(pType=="sBtBmsy"){
      fig.h()
    }else if(pType=="sEquilYield"){
      fig.i()
    }else if(pType=="sEquilF"){
      fig.j()
    #####################################    
    # Call figure code from pcod_iscamFigs.r #
    #####################################
    }else if(pType=="sCommAgeResids1"){
      fig.comm.age.residuals1()
    }else if(pType=="sCommAgeResids2"){
      fig.comm.age.residuals2()
    }else if(pType=="sCommAgeProps"){
      fig.comm.age.props()
    }else if(pType=="sSurvAgeResids1"){
      fig.survey.age.residuals1()
    }else if(pType=="sSurvAgeResids2"){
      fig.survey.age.residuals2()
    }else if(pType=="sSurvAgeProps"){
      fig.survey.age.props()
    }else if(pType=="sCommAgePropsFit"){
      fig.comm.age.props.fit()
    }else if(pType=="sSurvAgePropsFit"){
      fig.survey.age.props.fit()
    }else if(pType=="sSurvBiomassFit"){
      fig.surveybiomass.fit()
    }else if(pType=="sSelectivities"){
      fig.selectivity()
    }else if(pType=="sCatchFit"){
      fig.catchFit()
     }else if(pType=="sWeightFit"){
      fig.weightFit()
    }else if(pType=="sFishingMortality"){
      fig.fishingMortality()
    }else if(pType=="sRecAnom"){ #RF July 2012
      fig.RecAnomMPD()
    }else if(pType=="sRecAnomMCMC"){ #RF July 2012
      fig.RecAnomMCMC()
    }else if(pType=="sannualMeanWt"){ #RF July 2012
      fig.annualMeanWt()
    }else if(pType=="sPhase"){
      fig.phase()
    }else if(pType=="sTimeVaryingSurvSel"){
      fig.time.varying.selectivity(2)
    }else if(pType=="sTimeVaryingCommSel"){
      fig.time.varying.selectivity(1)
    }else if(pType=="sCtlPtsBox" && delaydiff==1){
        if(nyr>=2012) {fig.Allcontrol.pts.Box() 
      	 }else cat("No control point plot for 2005 bridging analyses\n")
    }else if(pType=="sMSYCtlPts" && delaydiff==1){
       if(nyr>=2012) {fig.MSYcontrol.pts()
       }else cat("No control point plot for 2005 bridging analyses\n")
    }else if(pType=="sHistCtlPts" && delaydiff==1){
        if(nyr>=2012) {fig.Histcontrol.pts()
	}else cat("No control point plot for 2005 bridging analyses\n")
    }else if(pType=="sBench" && delaydiff==1){
      if(nyr>=2012) {fig.Benchmarks()
      }else cat("No control point plot for 2005 bridging analyses\n")
     }else if(pType=="sCtlPtsBox" && delaydiff==0){
         cat("No control point plot for age-structured model\n")
   }else if(pType=="sMSYCtlPts" && delaydiff==0){
      cat("No control point plot for age-structured model\n")
    }else if(pType=="sHistCtlPts" && delaydiff==0){
        cat("No control point plot for age-structured model\n")
    }else if(pType=="sBench" && delaydiff==0){
      cat("No control point plot for age-structured model\n")  
    #################################################    
    # Call Parameter estimate code from pcod_iscamFigs.r #
    #################################################    
    }else if(pType=="sPosteriorParams"){
          fig.mcmc.priors.vs.posts(exFactor=1.0)
    }else if(pType=="sPosteriorParams2"){
          fig.mcmc.priors.vs.posts2(exFactor=1.0)
    }else if(pType=="sPosteriorParamskey"){
 	 fig.mcmc.priors.vs.postskey(exFactor=1.0)
    }else if(pType=="sParameterPairs"){
      fig.estimated.params.pairs()
    }else if(pType=="sParameterPairs2"){
      fig.estimated.params.pairs2()
    }else if(pType=="sParameterPairskey"){
      fig.estimated.params.pairs.key()
    }else if(pType=="sParameterPairsnologkey"){
      fig.estimated.params.pairs.no.log.key()
    }else if(pType=="sVariancePartitions"){
      fig.variance.partitions()
    }else if(pType=="sPriorsVsPosts"){
      fig.mcmc.priors.vs.posts(exFactor=1.0,showEntirePrior=T)
    }else if(pType=="sMCMCTrace"){
      fig.mcmc.trace()
    }else if(pType=="sMCMCAutocor"){
      fig.mcmc.autocor()
    }else if(pType=="sMCMCDensity"){
      fig.mcmc.density()
    }else if(pType=="sMCMCGeweke"){
      fig.mcmc.geweke()
    }else if(pType=="sMCMCGelman"){
      fig.mcmc.gelman()
    ###################################################    
    # Call Sensitivity plotting code from pcod_iscamSens.r #
    ###################################################
    }else if(pType=="sSensSB"){
      fig.base.vs.sens(sensitivityGroup=val$entrySensitivityGroup,
                       whichPlot="biomass",
                       ylimit=val$biomassYlim,
                       useMaxYlim=val$maxBiomassYlim,
                       offset=0.3)
    }else if(pType=="sSensD"){
      fig.base.vs.sens(sensitivityGroup=val$entrySensitivityGroup,
                       whichPlot="depletion",
                       ylimit=val$depletionYlim,
                       useMaxYlim=val$maxDepletionYlim)
    }else if(pType=="sSensRec"){
      fig.base.vs.sens(sensitivityGroup=val$entrySensitivityGroup,
                       whichPlot="recruits",
                       ylimit=val$recruitmentYlim,
                       useMaxYlim=val$maxRecruitmentYlim,
                       offset=0.3)
     }else if(pType=="sSensRefPts"){
      fig.base.vs.sens(sensitivityGroup=val$entrySensitivityGroup,
                       whichPlot="refpts",
                       ylimit=val$RefptSensYlim,
                       useMaxYlim=val$maxRefptSensYlim)                  
    ######################################################    
    # Call Retrospective plotting code from pcod_iscamRetro.r #
    ######################################################
    }else if(pType=="sRetroSB"){
      fig.retro(whichPlot="biomass",
                ylimit=val$biomassYlim,
                useMaxYlim=val$maxBiomassYlim)
    }else if(pType=="sRetroD"){
      fig.retro(whichPlot="depletion",
                ylimit=val$depletionYlim,
                useMaxYlim=val$maxDepletionYlim)
    }else if(pType=="sRetroRec"){
      fig.retro(whichPlot="recruits",
                ylimit=val$recruitmentYlim,
                useMaxYlim=val$maxRecruitmentYlim)      
    #############################################    
    # Call runtime Values code from pcod_iscamFigs.r #
    #############################################    
    }else if(pType=="sObjFuncVal"){
      plotRuntimeStats(1)
    }else if(pType=="sMaxGrad"){
      plotRuntimeStats(2)
    }else if(pType=="sFuncEvals"){
      plotRuntimeStats(3)
    }else if(pType=="sHangCodes"){
      plotRuntimeStats(4)
    }else if(pType=="sExitCodes"){
      plotRuntimeStats(5)
    }
  }
  par(oldpar)
  return(invisible())
}

.subView <- function(silent=F){
  act <- getWinAct()
  val <- getWinVal()
  # scenarioList is a list of the scenario names (i.e. folder names)
  scenarioList <- as.numeric(rownames(viewHeader))
  if(length(act)>1)
    act <- act[1]
  if(act=="prevScenario"){
    prevScenario <- val$entryScenario-1
    if(prevScenario<as.numeric(min(scenarioList))){
      prevScenario <- as.numeric(min(scenarioList))
    }
    setWinVal(c(entryScenario=prevScenario))
    assignGlobals(prevScenario)
    .loadPlottingLimits()
  }else if(act=="nextScenario"){
    nextScenario <- val$entryScenario+1
    if(nextScenario>as.numeric(max(scenarioList))){
      nextScenario <- as.numeric(max(scenarioList))
    }
    setWinVal(c(entryScenario=nextScenario))
    assignGlobals(nextScenario)
    .loadPlottingLimits()
  }else if(act=="changeEntryScenario"){
    assignGlobals(val$entryScenario)
    .loadPlottingLimits()    
  }else if(act=="prevSens"){
    prevSens <- val$entrySensitivityGroup - 1
    setWinVal(c(entrySensitivityGroup=prevSens))  
  }else if(act=="nextSens"){
    nextSens <- val$entrySensitivityGroup + 1
    setWinVal(c(entrySensitivityGroup=nextSens))  
  }else if(act=="writePlots"){
    .writePlots()
  }else if(act=="writeTables"){
    .writeTables()
  }else if(act=="writeAllPlots"){
    .writeAllPlots()
  }else if(act=="writeAllTables"){
    .writeAllTables()
  }else if(act=="writeSensPlots"){
    .writeSensPlots()
  }else if(act=="writeRetroPlots"){
    .writeRetroPlots()
  }else if(act=="runCurrScenario"){
    .runCurrScenario()
  }else if(act=="changeBurnThin"){
    .setBurnThin()
  }else if(act=="changeSelectivityGroup"){

  }else if(act=="changeSensStatus"){
    .writeSensitivityGroups()
  }else if(act=="runRetros"){
    .runRetros()
  }else if(act=="runAllRetros"){
    .runAllRetros()
  }else if(act=="changeBiomassYlim"){
    # Set the sensitivity and retro ylimit entry boxes and check boxes
    opList[[val$entryScenario]][[4]]$biomassYlim <<- val$biomassYlim
    opList[[val$entryScenario]][[4]]$maxBiomassYlim <<- val$maxBiomassYlim
    winList <- c(biomassSensYlim=val$biomassYlim,
                 maxBiomassSensYlim=val$maxBiomassYlim,
                 biomassRetroYlim=val$biomassYlim,
                 maxBiomassRetroYlim=val$maxBiomassYlim)
    try(setWinVal(winList), silent=silent)
  }else if(act=="changeBiomassSensYlim"){
    # Set the base and retro ylimit entry boxes and check boxes
    opList[[val$entryScenario]][[4]]$biomassYlim <<- val$biomassSensYlim
    opList[[val$entryScenario]][[4]]$maxBiomassYlim <<- val$maxBiomassSensYlim
    winList <- c(biomassYlim=val$biomassSensYlim,
                 maxBiomassYlim=val$maxBiomassSensYlim,
                 biomassRetroYlim=val$biomassSensYlim,
                 maxBiomassRetroYlim=val$maxBiomassSensYlim)
    try(setWinVal(winList), silent=silent)
  }else if(act=="changeBiomassRetroYlim"){
    # Set the base and sensitivity ylimit entry boxes and check boxes
    opList[[val$entryScenario]][[4]]$biomassYlim <<- val$biomassRetroYlim
    opList[[val$entryScenario]][[4]]$maxBiomassYlim <<- val$maxBiomassRetroYlim
    winList <- c(biomassYlim=val$biomassRetroYlim,
                 maxBiomassYlim=val$maxBiomassRetroYlim,
                 biomassSensYlim=val$biomassRetroYlim,
                 maxBiomassSensYlim=val$maxBiomassRetroYlim)
    try(setWinVal(winList), silent=silent)
  }else if(act=="changeDepletionYlim"){
    # Set the sensitivity and retro ylimit entry boxes and check boxes
    opList[[val$entryScenario]][[4]]$depletionYlim <<- val$depletionYlim
    opList[[val$entryScenario]][[4]]$maxDepletionYlim <<- val$maxDepletionYlim
    winList <- c(depletionSensYlim=val$depletionYlim,
                 maxDepletionSensYlim=val$maxDepletionYlim,
                 depletionRetroYlim=val$depletionYlim,
                 maxDepletionRetroYlim=val$maxDepletionYlim)
    try(setWinVal(winList), silent=silent)
  }else if(act=="changeDepletionSensYlim"){
    # Set the base and retro ylimit entry boxes and check boxes
    opList[[val$entryScenario]][[4]]$depletionYlim <<- val$depletionSensYlim
    opList[[val$entryScenario]][[4]]$maxDepletionYlim <<- val$maxDepletionSensYlim
    winList <- c(depletionYlim=val$depletionSensYlim,
                 maxDepletionYlim=val$maxDepletionSensYlim,
                 depletionRetroYlim=val$depletionSensYlim,
                 maxDepletionRetroYlim=val$maxDepletionSensYlim)
    try(setWinVal(winList), silent=silent)    
  }else if(act=="changeDepletionRetroYlim"){
    # Set the base and sensitivity ylimit entry boxes and check boxes
    opList[[val$entryScenario]][[4]]$depletionYlim <<- val$depletionRetroYlim
    opList[[val$entryScenario]][[4]]$maxDepletionYlim <<- val$maxDepletionRetroYlim
    winList <- c(depletionYlim=val$depletionRetroYlim,
                 maxDepletionYlim=val$maxDepletionRetroYlim,
                 depletionSensYlim=val$depletionRetroYlim,
                 maxDepletionSensYlim=val$maxDepletionRetroYlim)
    try(setWinVal(winList), silent=silent)
  }else if(act=="changeRecruitmentYlim"){
    # Set the sensitivity and retro ylimit entry boxes and check boxes
    opList[[val$entryScenario]][[4]]$recruitmentYlim <<- val$recruitmentYlim
    opList[[val$entryScenario]][[4]]$maxRecruitmentYlim <<- val$maxRecruitmentYlim
    winList <- c(recruitmentSensYlim=val$recruitmentYlim,
                 maxRecruitmentSensYlim=val$maxRecruitmentYlim,
                 recruitmentRetroYlim=val$recruitmentYlim,
                 maxRecruitmentRetroYlim=val$maxRecruitmentYlim)
    try(setWinVal(winList), silent=silent)
  }else if(act=="changeRecruitmentSensYlim"){
    # Set the base and retro ylimit entry boxes and check boxes
    opList[[val$entryScenario]][[4]]$recruitmentYlim <<- val$recruitmentYlim
    opList[[val$entryScenario]][[4]]$maxRecruitmentYlim <<- val$maxRecruitmentYlim
    winList <- c(recruitmentYlim=val$recruitmentSensYlim,
                 maxRecruitmentYlim=val$maxRecruitmentSensYlim,
                 recruitmentRetroYlim=val$recruitmentSensYlim,
                 maxRecruitmentRetroYlim=val$maxRecruitmentSensYlim)
    try(setWinVal(winList), silent=silent)
  }else if(act=="changeRecruitmentRetroYlim"){
    # Set the base and sensitivity ylimit entry boxes and check boxes
    opList[[val$entryScenario]][[4]]$recruitmentYlim <<- val$recruitmentRetroYlim
    opList[[val$entryScenario]][[4]]$maxRecruitmentYlim <<- val$maxRecruitmentRetroYlim
    winList <- c(recruitmentYlim=val$recruitmentRetroYlim,
                 maxRecruitmentYlim=val$maxRecruitmentRetroYlim,
                 recruitmentSensYlim=val$recruitmentRetroYlim,
                 maxRecruitmentSensYlim=val$maxRecruitmentRetroYlim)
    try(setWinVal(winList), silent=silent)
 }else if(act=="changeRefptSensYlim"){
      # Set the base and retro ylimit entry boxes and check boxes
      winList <- c(RefptSensYlim=val$RefptSensYlim,
                   maxRefptSensYlim=val$maxRefptSensYlim)
                   try(setWinVal(winList), silent=silent)
    }
  # Whichever radio button is selected will now be plotted for the scenario
  .doPlots()
}

.writeSensitivityGroups <- function(){
  val <- getWinVal()
  for(scenario in 1:length(opList)){
    filename <- paste(opList[[scenario]][[1]],fnSensitivityGroup,sep="")
    opList[[scenario]][[4]]$SensitivityGroup <<- val$scenarioHeader[scenario,2]
    write(val$scenarioHeader[scenario,2],filename)
  }
  cat("Saved the SensitivityGroup information.\n")
}

.runAllRetros <- function(silent=F){
  # Run retrospectives for all scenarios. Use value in entry box for years.
  val <- getWinVal()
  for(scenario in 1:length(opList)){
    .runRetros(scenario)
  }
  
}

.runRetros <- function(scenario=val$entryScenario,silent=F){
  # Steps:
  # 1. Save the current REP file by copying using file.copy
  # 2. Run the scenario using a system call for MLE retro, 1 for each retro year.
  # 3. Rename each of these runs' REP files to RET* where * is a number.
  # 4. Use file.copy to restore the original REP file
  
  val <- getWinVal()
  retroYears <- val$entryRetro
  showOutput <- val$showRetroOutput
  
  modelEXE <- exModel
  # Save the rscripts directory so we can get back to it
  rscriptsDir <- getwd()
  # change to this scenario's directory
  setwd(opList[[scenario]][[1]])
  # Save the REP file from the main non-retro run by copying to a backup file
  file.copy("pcod_iscam.rep","pcod_iscam.backup.rep")

  for(retro in 1:retroYears){
    modelCall <- paste(modelEXE,"-retro",retro)
    system(modelCall,wait=T,show.output.on.console=showOutput) 
    file.copy("pcod_iscam.rep",paste("pcod_iscam.ret",retro,sep=""),overwrite=T)
  }
  
  # Reinstantiate the REP file from the main non-retro run
  file.copy("pcod_iscam.backup.rep","pcod_iscam.rep",overwrite=T)
  
  setwd(rscriptsDir)
  loadScenario(scenario,silent=silent)
  assignGlobals(scenario)
}

.runCurrScenario <- function(scenario=val$entryScenario,deleteFiles=F,mleOverride=F,silent=F){
  # Steps:
  # 1. Copy the ADMB model executable to the current scenario's folder
  # 2. Run the scenario using a system call with either MLE or MCMC
  # 3. If MCMC, use another system call to run mceval
  
  goodToGo <- T
  val <- getWinVal()
  keepMCMC <- F
  if(val$executeType=="sMPD" || mleOverride){
    keepMCMC <- T
  }
  if(deleteFiles){
    deleteOldRun(scenario,keepMCMC=keepMCMC)
  }
  modelEXE <- exModel
  copyExecutableToScenarioFolder(scenario=scenario,silent=silent)

  rscriptsDir <- getwd() # Save the rscripts directory so we can get back to it
  setwd(opList[[scenario]][[1]])  # change to this scenario's directory
  if(val$delaydiff==1){  
	  	 
	  if(val$executeType=="sMPD" || mleOverride){
	    if(is.na(val$maxfn)){
	      modelCall <- paste(modelEXE, "-delaydiff")
	    }else{
	      modelCall <- paste(modelEXE,"-maxfn",val$maxfn, "-delaydiff")
	    }
	  }else if(val$executeType=="sMCMC"){
	    if(is.na(val$mcmc) || is.na(val$mcsave)){
	      cat("Error - check your mcmc and mcsave boxes for valid values.\n")
	      goodToGo <- F
	    }else{
	      mcevalCall <- paste(modelEXE,"-mceval", "-delaydiff")
	      if(is.na(val$maxfn)){
		modelCall <- paste(modelEXE,"-mcmc",val$mcmc,"-mcsave",val$mcsave, "-delaydiff")
	      }else{
		modelCall <- paste(modelEXE,"-mcmc",val$mcmc,"-mcsave",val$mcsave,"-maxfn",val$maxfn, "-delaydiff")
	      }
	    }
	  }
	}else { #end if delaydiff
	if(val$executeType=="sMPD" || mleOverride){
	    if(is.na(val$maxfn)){
	      modelCall <- modelEXE
	    }else{
	      modelCall <- paste(modelEXE,"-maxfn",val$maxfn)
	    }
	  }else if(val$executeType=="sMCMC"){
	    if(is.na(val$mcmc) || is.na(val$mcsave)){
	      cat("Error - check your mcmc and mcsave boxes for valid values.\n")
	      goodToGo <- F
	    }else{
	      mcevalCall <- paste(modelEXE,"-mceval")
	      if(is.na(val$maxfn)){
		modelCall <- paste(modelEXE,"-mcmc",val$mcmc,"-mcsave",val$mcsave)
	      }else{
		modelCall <- paste(modelEXE,"-mcmc",val$mcmc,"-mcsave",val$mcsave,"-maxfn",val$maxfn)
	      }
	    }
	  }
	} #end else delaydiff
  
  if(goodToGo){
    shell(modelCall)
    if(val$executeType=="sMCMC"){                                 
      shell(mcevalCall)
    }
  }
  setwd(rscriptsDir)
  loadScenario(scenario,silent=silent)
  assignGlobals(scenario,silent=silent)
  
}

.closeActWin <- function(){
  closeWin(.getWinName())
}

# .getWinName  (get the current winName)
# Purpose:     Determine which GUI is active (guiSim, guiView, guiPerf, etc.)
# Parameters:  None
# Returns:     A character containing the name of the current GUI window
# Source:      A.R. Kronlund, modified from PBSref (helper_funs.r)
.getWinName <- function(){
  win <- .PBSmod$.activeWin
  # This is only required if PBSask is used, leave it for now.
  if(win=="PBSask"){
    win <- getWinVal("win", winName="PBSask")[[1]]   # Hidden field in PBSask
    win <- gsub("\n", "", win)                       # Remove the linefeed \n
  }
  return(win)
}

cat("Type iscam() to start GUI\n")
cat("Optional arguments: pcod_iscam(reload=T,silent=F,copyADMBExecutables=F)\n\n")

