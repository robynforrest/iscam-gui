#**********************************************************************************
# iscam-gui.r
# This file contains the code for a front end GUI controller for iscam-gui using
# Tcl/Tk windows implemented using the R package 'PBSModelling'. The data
# structure used is a list, which is a list of lists, see iscam-gui-load-scenarios.r for
# details on this structure.
#
# Author            : Chris Grandin
# Development Date  : August 2013 - Present
# Current version   : 1.0
#
# Source this file, then call iscam()
#
# iscam(reload=F, silent=TRUE)
#
#**********************************************************************************

# TODO:
# Implement the order changes by sorting the op list, etc
# Add the ability to copy any one of the scenarios, at which time the gui will have to reload (how?)
# Fix all mcmc diagnostic plotting and other misc plotting.
# Remove all references to assignGlobals()

removeAllExcept <- function(vars  = c("op","sens","bio")){
  # Removes everyobject in the workspace except for what is in the vars list.
  # Upon finishing, the workspace will contain whatever is in the vars list,
  #  plus the objects 'removeAllExcept' (this function) and 'modelLoaded'.
  # That tells the software that the model has already been loaded.

  vars <- c(vars, "removeAllExcept")
  keep <- match(x = vars, table = ls(all = TRUE, envir = .GlobalEnv))
  if(any(is.na(keep))){
    modelLoaded <<- FALSE
  }else{
    rm(list=ls(all = TRUE, envir = .GlobalEnv)[-keep], envir = .GlobalEnv)
    modelLoaded <<- TRUE
  }
}
removeAllExcept()

require(PBSmodelling)
require(coda)
require(ggplot2) # Only used for Observed landings plot.
require(reshape2)
require(Hmisc)

options(stringsAsFactors = FALSE)
options(warn = -1)
source("iscam-gui-globals.r")
if(.OS == "Linux" || .OS == "Darwin"){
  # This stops PBSmodelling from complaining and erroring out in Linux
  #require("Tktable")
  # This changes the windows() function call for Linux to X11()
  windows <- function(...) X11(...)
  # This changes the shell() function call for Linux to system()
  shell   <- function(...) system(...)
}

# iscam-gui sources
source(.UTILITIES_SOURCE)
source(.LOAD_SCENARIOS_SOURCE)
source(.LOAD_BIODATA_SOURCE)
source(.FILE_CONTROL_SOURCE)
source(.REP_PARSER_SOURCE)
source(.FIGURES_SOURCE)
source(.FIGURES_BIOLOGY_SOURCE)
source(.FIGURES_SELEX_SOURCE)
source(.FIGURES_TIMESERIES_SOURCE)
source(.FIGURES_CATCH_SOURCE)
source(.FIGURES_MCMC_SOURCE)
source(.FIGURES_RETROSPECTIVES_SOURCE)

iscam <- function(reloadScenarios      = FALSE,
                  silent               = TRUE){
  # loads model outputs and launches the main iscam-gui GUI.
  # - reloadScenarios TRUE/FALSE - reload the data from all model output files in all scenarios.
  # - silent TRUE/FALSE - show messages on command line

  # Create a global variable which tells the program whether or not to be silent
  # This is the only capitalized, dotted variable not in iscam-gui-globals.r
  .SILENT <<- silent

  graphics.off()  # Destroy graphics window if it exists

  .loadData(reloadScenarios = reloadScenarios)

  if(!exists("sens")){
    sens <<- .loadSensitivityGroups(op = op)
  }
  dir.create(.SENS_FIGURES_DIR_NAME, showWarnings=FALSE)
  return(.GUIsetup("mainGui"))
}

.GUIsetup <- function(win, silent = .SILENT){
  if(win=="mainGui"){
    viewHeader            <<- data.frame()
    viewSensitivityGroups <<- data.frame()
    viewColor             <<- data.frame()
    viewLineType          <<- data.frame()
    for(scenario in 1:length(op)){
      viewHeader            <<- rbind(viewHeader,op[[scenario]]$names$scenario)
      viewSensitivityGroups <<- rbind(viewSensitivityGroups,op[[scenario]]$inputs$sensitivityGroup)
      viewColor             <<- rbind(viewColor,op[[scenario]]$inputs$color)
      #TODO: Change loading code so that the order is actually written as the linetype
      viewLineType          <<- rbind(viewLineType,op[[scenario]]$inputs$linetype)
    }
    colnames(viewHeader)            <<- .SCENARIO_LIST_LABEL
    colnames(viewSensitivityGroups) <<- .SENSITIVITY_GROUP_LABEL
    colnames(viewColor)             <<- .PLOT_COLOR_LABEL
    colnames(viewLineType)          <<- .PLOT_LINETYPE_LABEL
    scenarioHeader <<- cbind(viewHeader,viewSensitivityGroups,viewColor,viewLineType)
    scenarioList   <<- as.numeric(rownames(viewHeader))

    createWin(.MAIN_GUI_DEF_FILE,env=.GlobalEnv)
    winList <- c(entryScenario=1)
    try(setWinVal(winList), silent=silent)

    # TODO: Grey out currently unimplemented stuff
    #setWidgetState("","disabled")

    .updateGUIStamps(silent = silent)
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

.writeAllPlots <- function(silent=.SILENT){
  # write all figures for all scenarios to disk
  #scenarioList <- as.numeric(rownames(viewHeader))
  #for(scenario in scenarioList){
  #  assignGlobals(scenario)
  #  .writePlots(scenario)
  #}
}

.writeAllTables <- function(silent=.SILENT){
  # write all tables for all scenarios to disk
  #scenarioList <- as.numeric(rownames(viewHeader))
  #for(scenario in scenarioList){
  #  assignGlobals(scenario)
  #  .writeTables()
  #}
}

.writeRetroPlots <- function(silent=.SILENT){
  #assign("saveon",T,envir=.GlobalEnv)
  #val <- getWinVal()
  #fig.retro(whichPlot="biomass",
  #          ylimit=val$biomassYlim,
  #          useMaxYlim=val$maxBiomassYlim,
  #          scenario=val$entryScenario)
  #fig.retro(whichPlot="depletion",
  #          ylimit=val$depletionYlim,
  #          useMaxYlim=val$maxDepletionYlim,
  #          scenario=val$entryScenario)
  #fig.retro(whichPlot="recruits",
  #          ylimit=val$recruitmentYlim,
  #          useMaxYlim=val$maxRecruitmentYlim,
  #          scenario=val$entryScenario)
  #assign("saveon",FALSE,envir=.GlobalEnv)
}

.writeSensPlots <- function(silent=.SILENT){
  # write overlay sensitivity plots
  assignGlobals(1)
  assign("saveon",T,envir=.GlobalEnv)
  val <- getWinVal()
  uniqueSensitivityGroups <- c()  # base must be 0
  for(scenario in 1:length(op)){
    # count number of unique sensitivity groups
    if(!is.element(op[[scenario]][[4]]$SensitivityGroup,uniqueSensitivityGroups) && op[[scenario]][[4]]$SensitivityGroup != 0){
        uniqueSensitivityGroups <- c(uniqueSensitivityGroups,op[[scenario]][[4]]$SensitivityGroup)
    }
  }
  for(sensitivityGroup in uniqueSensitivityGroups){
    fig.base.vs.sens(sensitivityGroup=sensitivityGroup,
                     whichPlot="biomass",
                     ylimit=val$biomassYlim,
                     useMaxYlim=val$maxBiomassYlim)
    fig.base.vs.sens(sensitivityGroup=sensitivityGroup,
                     whichPlot="depletion",
                     ylimit=val$depletionYlim,
                     useMaxYlim=val$maxDepletionYlim)
    fig.base.vs.sens(sensitivityGroup=sensitivityGroup,
                     whichPlot="recruits",
                     ylimit=val$recruitmentYlim,
                     useMaxYlim=val$maxRecruitmentYlim)
  }
  assign("saveon",FALSE,envir=.GlobalEnv)
}

.doPlots <- function(savefig=.SAVEFIG){
  # savefig = TRUE means save the plots, FALSE means display on screen.
  graphics.off()
  .PLOT_IS_LIVE <<- FALSE

  val <- getWinVal()
  s   <- val$entryScenario
  if(val$compFitSex == "sCompFixSexC"){
    compFitSex <- 0
  }else if(val$compFitSex == "sCompFitSexM"){
    compFitSex <- 1
  }else if(val$compFitSex == "sCompFitSexF"){
    compFitSex <- 2
  }else{
    compFitSex <- 0
  }
  sgr <- val$entrySensitivityGroup
  ind <- val$entryIndex
  plotMCMC  <- val$plotMCMC
  ci        <- val$entryConfidence  # Confidence interval
  pType     <- val$viewPlotType
  # Plot Specs list for sizing of plots
  ps <- list(pngres  = val$entryResolution,
             pngw = val$entryWidth,
             pngh = val$entryHeight,
             res = val$entryResolutionScreen,
             w = val$entryWidthScreen,
             h = val$entryHeightScreen)
  burnthin <- c(val$burn, val$thin)

  if(val$figureType == "sEPS"){
    figtype <- .EPS_TYPE
  }
  if(val$figureType == "sPNG"){
    figtype <- .PNG_TYPE
  }

  if(val$legendLoc == "sLegendTopright"){
    leg <- "topright"
  }
  if(val$legendLoc == "sLegendTopleft"){
    leg <- "topleft"
  }
  if(val$legendLoc == "sLegendBotright"){
    leg <- "bottomright"
  }
  if(val$legendLoc == "sLegendBotleft"){
    leg <- "bottomleft"
  }
  if(val$legendLoc == "sLegendNone"){
    leg <- NULL
  }

  if(.checkEntries()){

   switch(pType,
           # From iscam-gui-figures-timeseries.r
           "sTSSpawningBiomassAllAreas"             = {plotTS(s,1,savefig,"SpawningBiomassAllAreas",plotMCMC,ci,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sTSSpawningBiomassByArea"               = {plotTS(s,2,savefig,"SpawningBiomassByArea",plotMCMC,ci,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sTSSpawningDepletionAllAreas"           = {plotTS(s,3,savefig,"SpawningDepletionAllAreas",plotMCMC,ci,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sTSSpawningDepletionByArea"             = {plotTS(s,4,savefig,"SpawningDepletionByArea",plotMCMC,ci,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sTSRecruitmentAllAreas"                 = {plotTS(s,5,savefig,"RecruitmentAllAreas",plotMCMC,ci,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sTSRecruitmentByArea"                   = {plotTS(s,6,savefig,"RecruitmentByArea",plotMCMC,ci,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           # Only MPD for Index
           "sTSIndex"                               = {plotTS(s,7,savefig,"Index",plotMCMC,ci,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sSPRRatio"                              = {plotTS(s,8,savefig,"SPRRatio",plotMCMC,ci,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           # Only MPD for Fishing mortality
           "sFishingMortality"                      = {plotTS(s,9,savefig,"Fishing Mortality",plotMCMC,ci,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sRefPoints"                             = {plotTS(s,10,savefig,"ReferencePoints",plotMCMC,ci,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           # From iscam-gui-figures-biology.r
           "sBiologyMeanWtAtAge"                    = {plotBiology(1,compFitSex,savefig,"BiologyMeanWtAtAge",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyMaturityAtAge"                  = {plotBiology(2,compFitSex,savefig,"BiologyMaturityAtAge",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyFecundity"                      = {plotBiology(3,compFitSex,savefig,"BiologyFecundity",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyFecundityWeight"                = {plotBiology(4,compFitSex,savefig,"BiologyFecundityWeight",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyFecundityLength"                = {plotBiology(5,compFitSex,savefig,"BiologyFecundityLength",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologySpawnOutputLength"              = {plotBiology(6,compFitSex,savefig,"BiologySpawnOutputLength",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyExpectedGrowth"                 = {plotBiology(7,compFitSex,savefig,"BiologyExpectedGrowth",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyTVM"                            = {plotBiology(8,compFitSex,savefig,"BiologyTVM",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyTVGrowthPersp"                  = {plotBiology(9,compFitSex,savefig,"BiologyTVGrowthPersp",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyTVGrowthContour"                = {plotBiology(10,compFitSex,savefig,"BiologyTVGrowthContour",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyComposition"                    = {plotBiology(11,compFitSex,savefig,"BiologyComposition",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyCompositionFit"                 = {plotBiology(12,compFitSex,savefig,"BiologyCompositionFit",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyCompositionResid"               = {plotBiology(13,compFitSex,savefig,"BiologyCompositionResiduals",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyLW"                             = {plotBiology(14,compFitSex,savefig,"BiologyLengthWeightRelationship",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyVONB"                           = {plotBiology(15,compFitSex,savefig,"BiologyVonBRelationship",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sBiologyMA"                             = {plotBiology(16,compFitSex,savefig,"BiologyMaturityAgeRelationship",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           # From iscam-gui-figures-selectivities.r
           #"sSelexLengthBasedByFleet"               = {plotSelex(1,savefig,"SelexLengthBasedByFleet",plotMCMC,ci,sensGroup=sgr,index=ind)},
           #"sSelexAgeBasedByFleet"                  = {plotSelex(2,savefig,"SelexAgeBasedByFleet",plotMCMC,ci,sensGroup=sgr,index=ind)},
           "sSelexLogisticByFleet"                  = {plotSelex(1,compFitSex,savefig,"SelexLogisticByFleet",plotMCMC,ci,sensGroup=sgr,index=ind,figtype=figtype)},
           # From iscam-gui-figures-catch.r
           "sCatchLandings"                         = {plotCatch(s,1,savefig,"CatchLandings",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           #"sCatchLandingsStacked"                  = {plotCatch(s,2,savefig,"CatchLandingsStacked",plotMCMC,ci,sensGroup=sgr,index=ind)},
           "sCatchLandingsObsVsExpLandings"         = {plotCatch(s,3,savefig,"CatchLandingsObsVsExpLandings",plotMCMC,ci,sensGroup=sgr,index=ind,ps=ps,leg=leg,figtype=figtype)},
           "sCatchAnnualMeanWt" 		                = {plotCatch(s,4,savefig,"CatchFitAnnualMeanWeight",plotMCMC,ci,sensGroup=sgr,index=ind,figtype=figtype)},
           # MCMC diagnostics, convergence, and parameter plots
           # From iscam-gui-figures-mcmc-convergence.r
           "sMCMCTrace"                             = {plotConvergence(s,1,savefig,"Trace",ps=ps,burnthin=burnthin,figtype=figtype)},
           "sMCMCAutocor"                           = {plotConvergence(s,2,savefig,"Autocor",ps=ps,burnthin=burnthin,figtype=figtype)},
           "sMCMCDensity"                           = {plotConvergence(s,3,savefig,"Density",ps=ps,burnthin=burnthin,figtype=figtype)},
           "sParameterPairs"                        = {plotConvergence(s,4,savefig,"Pairs",ps=ps,burnthin=burnthin,figtype=figtype)},
           "sPriorsVsPosts"                         = {plotConvergence(s,5,savefig,"PriorsPosteriors",ps=ps,burnthin=burnthin,figtype=figtype,exFactor=1.5,showEntirePrior=T)},
           "sVariancePartitions"                    = {plotConvergence(s,6,savefig,"VariancePartitions",ps=ps,burnthin=burnthin,figtype=figtype)},
           #"sMCMCGeweke"                            = {fig.mcmc.geweke(scenario=val$entryScenario)},
           #"sMCMCGelman"                            = {fig.mcmc.gelman(scenario=val$entryScenario)},
           # From iscam-gui-figures-timeseries.r
           "sSensSB"                                = {plotTS(s,1,savefig,"SpawningBiomass",plotMCMC,ci,multiple=TRUE,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sSensBRatio"                            = {plotTS(s,3,savefig,"Depletion",plotMCMC,ci,multiple=TRUE,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sSensRecruit"                           = {plotTS(s,5,savefig,"Recruitment",plotMCMC,ci,multiple=TRUE,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype,recrOffset=val$entryRecrOffset)},
           # No sensitivity plot for MCMC Indices
           "sSensIndex"                             = {plotTS(s,7,savefig,"Index",FALSE,ci,multiple=TRUE,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           #"sSensSPRRatio"                          = {plotTS(7,savefig,"SPRRatio",plotMCMC,ci,TRUE,btarg=val$entryBtarg,blim=val$entryBlim)},
           #"sSensRecruitU"                          = {plotTS(8,savefig,"RecruitUncertainty",plotMCMC,ci,TRUE)},
           # No sensitivity plot for MCMC Fs yet, it would likely be too busy anyway
           "sSensF"                                 = {plotTS(s,9,savefig,"MeanF",FALSE,ci,multiple=TRUE,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sSensRefPoints"                         = {plotTS(s,10,savefig,"ReferencePoints",plotMCMC,ci,multiple=TRUE,sensGroup=sgr,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           #"sSensRecruitDev"                        = {plotTS(9,savefig,"RecruitmentDev",plotMCMC,ci,TRUE)},
           #"sSensIndexLog"                          = {plotTS(12,savefig,"IndexLog",plotMCMC,ci,TRUE)},
           #"sSensDensity"                           = {plotTS(13,savefig,"Density",plotMCMC,ci,TRUE)},
           # Plot Retrospectives
           "sRetroSB"                               = {plotTS(s,1,savefig,"RetroSpawningBiomass",retros=TRUE,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sRetroD"                                = {plotTS(s,3,savefig,"RetroDepletion",retros=TRUE,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sRetroRec"                              = {plotTS(s,5,savefig,"RetroSpawningBiomass",retros=TRUE,index=ind,burnthin=burnthin,ps=ps,leg=leg,figtype=figtype)},
           "sRetroSquid"                            = {plotCohorts(s,savefig=savefig,fileText="RetroSquid",ps=ps,leg=leg,figtype=figtype)},
           # Plot runtime values returned from ADMB
           "sObjFuncVal"                            = {plotConvergence(1,savefig,"ObjectiveFunctionValue",burnthin=burnthin,figtype=figtype)},
           "sMaxGrad"                               = {plotConvergence(2,savefig,"MaximumGradient",burnthin=burnthin,figtype=figtype)},
           "sFuncEvals"                             = {plotConvergence(3,savefig,"FunctionEvaluations",burnthin=burnthin,figtype=figtype)},
           "sHangCodes"                             = {plotConvergence(4,savefig,"HangCodes",burnthin=burnthin,figtype=figtype)},
           "sExitCodes"                             = {plotConvergence(5,savefig,"ExitCodes",burnthin=burnthin,figtype=figtype)},
           {
             # Default
           }
           )
  }
  .PLOT_IS_LIVE <<- TRUE
  return(invisible())
}

.subView <- function(savefig=.SAVEFIG, silent = .SILENT){
  act <- getWinAct()
  val <- getWinVal()
  triggerPlot <- TRUE # this triggers plotting upon completion of this call
  scenario <- val$entryScenario
  # scenarioList is a list of the scenario names (i.e. folder names)
  scenarioList <- as.numeric(rownames(viewHeader))
  # Could sort this but there's no need we are just looking at max and min below
  sensList <- unique(as.numeric(val$scenarioHeader$Group))
  if(length(act)>1){
    act <- act[1]
  }
  currFuncName <- getCurrFunc()
  # This switch statement represents an 'action' for a button or changing a text field.
  # See iscam-gui-gui-specs.r
  triggerPlot <- FALSE

  switch(act,
         # Change the scenario number using three different methods
         "prevScenario" = {
           prevScenario <- val$entryScenario-1
           if(prevScenario<as.numeric(min(scenarioList))){
             prevScenario <- as.numeric(min(scenarioList))
           }
           winValList <- c(entryScenario=prevScenario)
           setWinVal(winValList)
           .updateGUIStamps()
         },
         "nextScenario" = {
           nextScenario <- val$entryScenario+1
           if(nextScenario>as.numeric(max(scenarioList))){
             nextScenario <- as.numeric(max(scenarioList))
           }
           winValList <- c(entryScenario=nextScenario)
           setWinVal(winValList)
           .updateGUIStamps(silent = silent)
         },
         "changeEntryScenario" = {
           if(is.na(val$entryScenario)){
             setWinVal(c(entryScenario=min(scenarioList)))
           }
           if(val$entryScenario < min(scenarioList)){
             setWinVal(c(entryScenario=min(scenarioList)))
           }else if(val$entryScenario > max(scenarioList)){
             setWinVal(c(entryScenario=max(scenarioList)))
           }
           .updateGUIStamps()
         },
         "reloadScenario" = {
           val <- getWinVal()
           op[[scenario]] <<- .loadScenario(scenario, dired=op[[scenario]]$names$dir)
           .updateGUIStamps()
         },
         # Edit the various files which are unique to each scenario
         "editControlFile" = {
           #.controlfileGUISetup()
           .editFile(scenario = val$entryScenario, type=1)
         },
         "editDataFile" = {
           .editFile(scenario = val$entryScenario, type=2)
         },
         "editStarterFile" = {
           .editFile(scenario = val$entryScenario, type=3)
         },
         "editCurrScenarioLogfile" = {
           .editFile(scenario = val$entryScenario, type=4)
         },
         "editProjectionFile" = {
           .editFile(scenario = val$entryScenario, type=5)
         },
         "editWarningfile" = {
           .editFile(scenario = val$entryScenario, type=6)
         },
         "editParFile" = {
           .editFile(scenario = val$entryScenario, type=7)
         },
         "editReportFile" = {
           .editFile(scenario = val$entryScenario, type=8)
         },
         # These are from the 'RunModel' tab
         "refreshLogfileTimestamps" = {
           .updateGUIStamps()
         },
         "modelChangeMPD" = {
           .updateGUICommandStamp()
         },
         "modelChangeMCMC" = {
           .updateGUICommandStamp()
         },
         "mcmcValChanged" = {
           .updateGUICommandStamp()
         },
         "mcsaveValChanged" = {
           .updateGUICommandStamp()
         },
         "maxfnValChanged" = {
           .updateGUICommandStamp()
         },
         # Several ways to change the sensitivity group number
         "prevSens" = {
           prevSens <- val$entrySensitivityGroup - 1
           if(prevSens < min(sensList)){
             prevSens <- min(sensList)
           }
           setWinVal(c(entrySensitivityGroup=prevSens))
         },
         "nextSens" = {
           nextSens <- val$entrySensitivityGroup + 1
           if(nextSens > max(sensList)){
             nextSens <- max(sensList)
           }
           setWinVal(c(entrySensitivityGroup=nextSens))
         },
         # Several ways to change the index or gear group number
         "prevGroup" = {
           prevGroup <- val$entryIndex - 1
           if(prevGroup < 1){
             prevGroup <- 1
           }
           setWinVal(c(entryIndex=prevGroup))
         },
         "nextGroup" = {
           nextGroup <- val$entryIndex + 1
           if(nextGroup > length(op[[scenario]]$inputs$data$indices)){
             nextGroup <- length(op[[scenario]]$inputs$data$indices)
           }
           setWinVal(c(entryIndex=nextGroup))
         },
         # Write the plots and tables to disk
         "writePlots" = {
           .writePlots(val$entryScenario)
         },
         "writeTables" = {
           .writeTables()
         },
         "writeAllPlots" = {
           .writeAllPlots()
         },
         "writeAllTables" = {
           .writeAllTables()
         },
         "writeSensPlots" = {
           .writeSensPlots()
         },
         "writeRetroPlots" = {
           .writeRetroPlots()
         },
         "runCurrScenario" = {
           runMCMC <- FALSE
           if(!is.na(val$mcmc)){
             runMCMC <- TRUE  # Need this to tell runCurrScenario to do the mceval step.
           }
           if(.runCurrScenario(runMCMC = runMCMC)){
             val <- getWinVal()
             scenario <- val$entryScenario
             op[[scenario]] <<- .loadScenario(scenario, dired=op[[scenario]]$names$dir)
             sens <<- .loadSensitivityGroups(op = op)
             .updateGUIStamps()
             alarm() # Sound an alarm to notify user that run is finished
           }
         },
         "cleanDirectory" = {
           if(.deleteOutputs(val$entryScenario)){
             .removeConvergenceValues()
             .updateGUIStamps()
           }
         },
         "changeBurnThin" = {
         },
         "changeRecrOffset" = {
         },
         "changeEndyrvec" = {
         },
         "changeCohort" = {
         },
         "changeSensStatus" = {
           # Rewrite all scenarioInfo files to reflect change
           val <- getWinVal()
           for(row in 1:nrow(val$scenarioHeader)){
             .createScenarioInfoFile(dired = op[[row]]$names$dir,
                                     scenario = row,
                                     default = FALSE)
             op[[row]]$inputs$color <<- val$scenarioHeader$Color[[row]]
             op[[row]]$inputs$linetype <<- val$scenarioHeader$Line[[row]]
             op[[row]]$inputs$sensitivityGroup <<- val$scenarioHeader$Group[[row]]
             cat("\n")
           }
           sens <<- .loadSensitivityGroups(op = op)
         },
         "changeScreenGraphics" = {
         },
         "changeConfidence" = {
         },
         "runRetros" = {
           .runRetros()
           # Reload this scenario, which does a recursive load of the retrospective runs
           cat0(.PROJECT_NAME,"->",currFuncName,"Loading output from retrospective runs..\n")
           op[[scenario]] <<- .loadScenario(scenario, dired=op[[scenario]]$names$dir)
           .updateGUIStamps()
           alarm() # Sound an alarm to notify user that runs are finished
         },
         "runAllRetros" = {
           .runAllRetros()
         },
         "saveCurrFigure" = {
           .doPlots(savefig=TRUE)
         },
         "changeBtarg" = {
           .doPlots(savefig=savefig)
         },
         "changeBlim" = {
           .doPlots(savefig=savefig)
         },
         "openBioDataFile" = {
           scenario <- val$entryScenario
           scenarioDir <- op[[scenario]]$names$dir
           biodataFile <<- selectFile(initialdir = .BIODATA_DIR_NAME, filetype = .BIODATA_FILE_TYPES)
           if(is.null(biodataFile)){
             biodataFile <<- ""
           }
           setWinVal(c(entryBioDataFile=biodataFile))
           .loadBiodata()
         },
         "showSurveyList" = {
           print(surveyList)
         },
         "editLWTPL" = {
           fn <- file.path(.BIODATA_DIR_NAME, .LW_EXE_BASE_NAME, .LW_TPL_FILE_NAME)
           editCall <- paste(.EDITOR, fn)
           shell(editCall, wait=F)
         },
         "editVONBTPL" = {
           fn <- file.path(.BIODATA_DIR_NAME, .VONB_EXE_BASE_NAME, .VONB_TPL_FILE_NAME)
           editCall <- paste(.EDITOR, fn)
           shell(editCall, wait=F)
         },
         "editMATPL" = {
           fn <- file.path(.BIODATA_DIR_NAME, .MA_EXE_BASE_NAME, .MA_TPL_FILE_NAME)
           editCall <- paste(.EDITOR, fn)
           shell(editCall, wait=F)
         },
         "runBio" = {
           if(!exists("biodata", envir = .GlobalEnv)){
             cat0(.PROJECT_NAME,"->",getCurrFunc(),"Global object 'biodata' does not exist. Load a datafile and try again.")
             return(NULL)
           }
           scenario <- val$entryScenario
           ages  <- .parseAges(val$entryAges)
           areas <- .parseAreas(val$entryAreas)
           surveys <- .parseSurveys(val$entrySurveys)
           # Assumes surveyKeys exists globally!
           #survey <- surveyKeys[val$dlSurveyList.id]
           splitSex <- FALSE
           if(val$sexType == "sSplit"){
             splitSex <- TRUE
           }
           # model = 1 means it is a length weight model
           model <- 1
           if(val$lwvType == "sVB"){
             # model = 2 means it is a vonB model
             model <- 2
           }
           if(val$lwvType == "sMA"){
             # model = 3 means it is a maturity/age model
             model <- 3
           }
           .runBioModel(model = model, ages = ages,
                        areas = areas, splitSex = splitSex, surveys = surveys,
                        multLen = val$entryLengthMult,
                        multWt  = val$entryWeightMult)
         },
         {
           triggerPlot <- TRUE
           # Default
         }
         )

  # Whichever radio button is selected will now be plotted for the scenario
  if(triggerPlot || (!is.null(dev.list()))){
    .doPlots(savefig=savefig)
  }
}

.setTimestampsRunning <- function(type, silent = .SILENT){
  # Set appropriate timestamp on the main gui to say 'Running model...'
  # Either MPD (1) or MCMC (2)

  if(type == 1){
    winList <- c(mpdTimestamp    = "Running model...",
                 mcmcTimestamp   = "",
                 mcevalTimestamp = "",
                 warningsText    = "",
                 numParams       = "",
                 objFunValue     = "",
                 maxGradient     = "")
  }
  if(type == 2){
    winList <- c(mpdTimestamp    = "",
                 mcmcTimestamp   = "Running model...",
                 mcevalTimestamp = "",
                 warningsText    = "",
                 numParams       = "",
                 objFunValue     = "",
                 maxGradient     = "")
  }
  try(setWinVal(winList), silent=silent)
}

.setTimestamps <- function(silent = .SILENT){
  #  Refresh the timestamp boxes in the GUI to the values

  winList            <- NULL
  val                <- getWinVal()
  scenario           <- val$entryScenario
  op[[scenario]]$inputs$log <- .loadLogfile(dired = op[[scenario]]$names$dir)
  loadLogFileSuccess <- op[[scenario]]$inputs$log$isMPD || op[[scenario]]$inputs$log$isMCMC

  if(!loadLogFileSuccess){
    # Logfile failed to load right
    winList <- c(winList,
                 mpdTimestamp    = "No logfile found",
                 mcmcTimestamp   = "No logfile found",
                 mcevalTimestamp = "No logfile found",
                 warningsText    = "")

  }else if(loadLogFileSuccess &&
           !op[[scenario]]$inputs$log$isMCMC){
    winList <- c(winList,
                 mpdTimestamp    = op[[scenario]]$inputs$log$finishTimes[1],
                 mcmcTimestamp   = "",
                 mcevalTimestamp = "",
                 warningsText    = op[[scenario]]$inputs$log$hessianWarning)
  }else if(loadLogFileSuccess){
    winList <- c(val$winList,
                 mpdTimestamp    = "",
                 mcmcTimestamp   = op[[scenario]]$inputs$log$finishTimes[1],
                 mcevalTimestamp = "MCEval not run",
                 warningsText    = op[[scenario]]$inputs$log$hessianWarning)
    if(op[[scenario]]$inputs$log$hasMCeval){
      winList <- c(winList,
                   mpdTimestamp    = "",
                   mcmcTimestamp   = op[[scenario]]$inputs$log$finishTimes[1],
                   mcevalTimestamp = op[[scenario]]$inputs$log$finishTimes[2],
                   warningsText    = op[[scenario]]$inputs$log$hessianWarning)
    }
  }
  try(setWinVal(winList), silent=silent)
}

.setConvergenceChecks <- function(silent = .SILENT){
  # Set the GUI textboxes with the par convergence values
  val                <- getWinVal()
  scenario           <- val$entryScenario
  winList <- c(numParams   = op[[scenario]]$outputs$par$numParams,
               objFunValue = op[[scenario]]$outputs$par$objFunValue,
               maxGradient = op[[scenario]]$outputs$par$maxGradient)
  try(setWinVal(winList), silent=silent)
}

.setLastCommandLine <- function(silent = .SILENT){
  # Set the GUI textboxes with the command line options last used
  val                <- getWinVal()
  scenario           <- val$entryScenario
  .loadLastCommandRunFile(scenario)
  # If the following is null, nothing has been done yet
  winList <- c(maxfn   = op[[scenario]]$inputs$lastCommandLine$maxfn,
               mcmc    = op[[scenario]]$inputs$lastCommandLine$mcmc,
               mcsave  = op[[scenario]]$inputs$lastCommandLine$mcsave,
               mcseed  = op[[scenario]]$inputs$lastCommandLine$mcseed,
               mno     = op[[scenario]]$inputs$lastCommandLine$mno,
               mcscale = op[[scenario]]$inputs$lastCommandLine$mcscale,
               maxph   = op[[scenario]]$inputs$lastCommandLine$maxph,
               mcrb    = op[[scenario]]$inputs$lastCommandLine$mcrb,
               mcprobe = op[[scenario]]$inputs$lastCommandLine$mcprobe,
               gbs     = op[[scenario]]$inputs$lastCommandLine$gbs,
               crit    = op[[scenario]]$inputs$lastCommandLine$crit,
               ams     = op[[scenario]]$inputs$lastCommandLine$ams,
               phase   = op[[scenario]]$inputs$lastCommandLine$phase,
               cbs     = op[[scenario]]$inputs$lastCommandLine$cbs,
               mdl     = op[[scenario]]$inputs$lastCommandLine$mdl)
  try(setWinVal(winList), silent=silent)
}

.setScenarioNames <- function(silent = .SILENT){
  #val <- getWinVal()
  #winList <- c(scenarioHeader = viewHeader)
  #try(setWinVal(winList))
}

.updateGUIStamps <- function(silent = .SILENT){
  .setTimestamps()
  .setConvergenceChecks()
  .setScenarioNames()
  .setLastCommandLine()
}

.removeConvergenceValues <- function(silent = .SILENT){
  # Set the convergence values to an empty string.  This is typically done
  # when the deleteOutputs function is called so that the .updateGUI
  # function has null strings with which to update the convergence
  # values in the GUI.
  val <- getWinVal()
  scenario <- val$entryScenario
  op[[scenario]]$outputs$par$numParams   <<- NA
  op[[scenario]]$outputs$par$objFunValue <<- NA
  op[[scenario]]$outputs$par$maxGradient <<- NA
}

.updateGUICommandStamp <- function(silent = .SILENT){
  val <- getWinVal()
  scenario <- val$entryScenario
  command <- .EXE_FILE_NAME
  if(val$executeType == "sMCMC"){
    if(!is.na(val$mcmc)){
      command <- paste(command,"-mcmc",val$mcmc)
      if(!is.na(val$mcsave)){
        command <- paste(command,"-mcsave",val$mcsave)
      }
    }
  }
  if(!is.na(val$maxfn)){
    command <- paste(command,"-maxfn",val$maxfn)
  }
  op[[scenario]]$inputs$lastCommandRun <- command
  try(setWinVal(c(runCommandText = op[[scenario]]$inputs$lastCommandRun)), silent=silent)
}

.runAllRetros <- function(silent = .SILENT){
  # Run retrospectives for all scenarios. Use value in entry box for years.
  val <- getWinVal()
  for(scenario in 1:length(op)){
    .runRetros(scenario)
  }
}

.runRetros <- function(scenario = val$entryScenario, silent = .SILENT){
  # Run retrospectives for the given scenario.
  # First you must agree to delete any old ones
  # Subdirectories 'RestrospectiveXX' will be created where XX is the number
  #  of years subtracted.

  val <- getWinVal()
  retroYears <- val$entryRetro
  showOutput <- val$showRetroOutput
  srcDir <- op[[scenario]]$names$dir
  currFuncName <- getCurrFunc()
  overwrite <- getYes(paste0("Warning, any retrospectives previously run for the '",op[[scenario]]$names$scenario,
                             "' scenario will be deleted if they exist. Continue?"),title="Proceed?",icon="question")
  if(!overwrite){
    cat0(.PROJECT_NAME,"->",currFuncName,"Aborting retrospective runs.\n")
    return(NULL)
  }
  # Delete all retrospective directories recursively
  for(retro in 1:retroYears){
    destDir <- file.path(srcDir,paste0(.RETRO_DIR_BASE,retro))
    unlink(destDir, recursive = TRUE, force = TRUE)
  }
  for(retro in 1:retroYears){
    # Copy the current model's directory 'retroYears' times, with
    #  each new directory being the number of years subtracted
    destDir <- file.path(srcDir,paste0(.RETRO_DIR_BASE,retro))
    files <- dir(srcDir)
    for(ind in 1:length(.OUTPUT_FILES)){
      # Remove any output files from the copy source so as not to mess up the retro directory
      pattern <- .OUTPUT_FILES[ind]
      # Replace * wildcard with an alphnumeric wildcard
      pattern <- sub("\\*","[[:alnum:]]+",pattern)
      gr <- grep(pattern, files)
      if(length(gr) > 0){
        files <- files[-gr]
      }
    }
    # Remove Figures and Tables directories
    gr <- grep(.FIGURES_DIR_NAME,files)
    if(length(gr) > 0){
      files <- files[-gr]
    }
    gr <- grep(.TABLES_DIR_NAME,files)
    if(length(gr) > 0){
      files <- files[-gr]
    }

    # Add scenarioInfo file if it is not still there
    gr <- grep(.SCENARIO_INFO_FILE_NAME, files)
    if(length(gr) == 0){
      gr <- grep(.SCENARIO_INFO_FILE_NAME, dir(srcDir))
      if(length(gr) > 0){
        # The ScenarioInfo file is in the srcDir, but not in our file list, so add it
        files <- c(files,.SCENARIO_INFO_FILE_NAME)
      }
    }
    srcFiles  <- file.path(srcDir,files)
    destFiles <- file.path(destDir,files)
    dir.create(destDir, showWarnings=FALSE)
    file.copy(srcFiles, destFiles, overwrite=TRUE, recursive=TRUE)
  }

  # Save the rscripts directory so we can get back to it
  currDir <- getwd()

  cat(.PROJECT_NAME,"->",currFuncName,"'",op[[scenario]]$names$scenario,"' scenario retrospective runs started.\n\n",sep="")
  cat("Don't touch the GUI - wait until a message appears stating the retrospective runs have finished.\n\n")
  tcl("update") # updates window text
  for(retro in 1:retroYears){
    retroDir <- file.path(currDir, op[[scenario]]$names$dir, paste0(.RETRO_DIR_BASE,retro))
    # change to this scenario's directory
    setwd(retroDir)
    modelCall <- .EXE_FILE_NAME
    if(.OS == "Linux" || .OS == "Darwin"){
      modelCall <- paste0("./",modelCall)
    }
    modelCall <- paste(modelCall,"-retro",retro)
    modelCall <- paste(modelCall, .DOS_PIPE_TO_LOG)

    cat0(.PROJECT_NAME,"->",currFuncName,"Running retrospective\nScenario: ",
         op[[scenario]]$names$scenario,"\nRetroyear: -",retro,"\n")
    shell(modelCall)
  }
  cat0(.PROJECT_NAME,"->",currFuncName,"'",op[[scenario]]$names$scenario,
      "' scenario retrospective runs finished.\nCheck the logfile for command line output.\n")
  setwd(currDir)
}

.buildModelCall <- function(scenario, silent = .SILENT){
  # Build up a string with the command line model call based on the GUI inputs
  # Returns the build string, or NULL if there was a problem with the entries in the GUI
  val <- getWinVal()
  commandLine <- op[[scenario]]$inputs$lastCommandLine

  # Sanity checks...
  #if(is.na(commandLine$mcmc) && !is.na(commandLine$mcsave)){
  #  cat(".buildModelCall: Error - check your mcmc and mcsave boxes for correct values.\n")
  #  return(NULL)
  #}
  # Sanity checks end..
  modelCall <- .EXE_FILE_NAME
  if(.OS == "Linux" || .OS == "Darwin"){
    modelCall <- paste0("./",modelCall)
  }
  for(command in 1:length(commandLine)){
    if(!is.na(commandLine[command])){
      modelCall <- paste0(modelCall, " -", names(commandLine[command]), " ", commandLine[command])
    }
  }
  modelCall <- paste(modelCall, .DOS_PIPE_TO_LOG)
  return(modelCall)
}

.runCurrScenario <- function(scenario    = val$entryScenario,
                             runMCMC     = FALSE,
                             silent      = .SILENT){
  # Deletes the old model run outputs from the scenario's directory.
  # Copies the model executable to the current scenario's folder,
  # runs the scenario using a system call for either MLE or MCMC
  # If runMCMC = TRUE, then use a second system call to run mceval.
  # Returns TRUE if the model was run, FALSE otherwise

  val          <- getWinVal()
  shellSuccess <- FALSE

  if(.deleteOutputs(scenario)){
    # Make sure the current values in the GUI for the command line are saved
    # in the scenario 'op' list.
    .setupCommandLineFromGUI()
    modelArgsGood <- TRUE
    .copyExecutableToScenarioDirectory(scenario = scenario)

    rscriptsDir   <- getwd()        # Save the rscripts full path directory so we can get back to it
    setwd(op[[scenario]]$names$dir) # change to this scenario's directory

    # Build command line here
    modelCall <- .buildModelCall(scenario)

    if(is.null(modelCall)){
      modelArgsGood <- FALSE
    }
    if(modelArgsGood){
      winList <- c(runCommandText = modelCall)
      try(setWinVal(winList))

      if(is.na(val$mcmc)){
        .setTimestampsRunning(type = 1)
      }else{
        .setTimestampsRunning(type = 2)
      }
      cat(.PROJECT_NAME,"->",getCurrFunc(),"'",op[[scenario]]$names$scenario,"' scenario model run started.\n\n",sep="")
      cat("Don't touch the GUI - wait until a message appears stating the run has finished.\n\n",
          "The command being run is:\n",
          modelCall,"\n\n")
      tcl("update") # updates window text

      if(shell(modelCall) == 0){
        shellSuccess <- TRUE
      }
      if(runMCMC){
        mcevalCall <- paste(.EXE_FILE_NAME,"-mceval",.DOS_APPEND_TO_LOG)
        if(.OS == "Linux" || .OS == "Darwin"){
          mcevalCall <- paste0("./", mcevalCall)
        }
        cat(.PROJECT_NAME,"->",getCurrFunc(),"MCeval phase, the command being run is:\n",
            mcevalCall,"\n\n",sep="")
        shell(mcevalCall)
      }
      logFileFullPath <- file.path(op[[scenario]]$names$dir,.LOG_FILE_NAME)
      cat(.PROJECT_NAME,"->",getCurrFunc(),"'",op[[scenario]]$names$scenario,
          "' scenario model run finished.\nCheck the logfile for command line output: '",logFileFullPath,"',\n",sep="")
    }
    setwd(rscriptsDir)
    if(shellSuccess){
      .writeLastCommandRunFile(scenario)
    }
    return(TRUE)
  }
  return(FALSE)
}

.closeActWin <- function(silent = .SILENT){
  closeWin(.getWinName())
}

# .getWinName  (get the current winName)
# Purpose:     Determine which GUI is active (guiSim, guiView, guiPerf, etc.)
# Parameters:  None
# Returns:     A character containing the name of the current GUI window
# Source:      A.R. Kronlund, modified from PBSref (helper_funs.r)
.getWinName <- function(silent = .SILENT){
  win <- .PBSmod$.activeWin
  # This is only required if PBSask is used, leave it for now.
  if(win=="PBSask"){
    win <- getWinVal("win", winName="PBSask")[[1]]   # Hidden field in PBSask
    win <- gsub("\n", "", win)                       # Remove the linefeed \n
  }
  return(win)
}

cat(.TELL_USER_HOW_TO_START_GUI)
cat(.TELL_USER_ABOUT_GUI_ARGUMENTS)

