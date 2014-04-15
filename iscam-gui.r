#**********************************************************************************
# iscam-gui.r
# This file contains the code for a front end GUI controller for iscam-gui using
# Tcl/Tk windows implemented using the R package 'PBSModelling'.  The data
# structure used is an list, which is a list of lists, see iscam-gui-load-scenarios.r for
# details on this structure. This file assumes that a list object called 'op'
# exists and is of the correct format.
#
# Author            : Chris Grandin
# Development Date  : August 2013 - Present
# Current version   : 1.0
#
# Source this file, then call iscam()
#
# iscam(reload=F, silent=TRUE, copyModelExecutables = FALSE)
#
#**********************************************************************************

# TODO:
# Fix retrospective plotting and model running, need an mcmc buttton as weel for retros
# Implement the order changes by sorting the op list, etc
# Fix the sensitivity changnig so that it can't go past limits
# Change the radios to check boxes for plotting, with all checked appearing in a table, i.e. mfrow, mfcol
#  also there should be a radio button for "all side-by-side", "all top-bottom", or "as square as possible" etc
# Add the ability to copy any one of the scenarios, at which time the gui will have to reload (how?)
# Fix all mcmc diagnostic plotting and other misc plotting.
# Remove all references to assignGlobals()
  
removeAllExcept <- function(vars  = c("op","sens")){
  # removeAllExcept()
  # Removes everything in the workspace except for what is in the vars list.
  # Upon finishing, the workspace will contain whatever is in the vars list,
  # plus the objects 'removeAllExcept' (this function) and 'modelLoaded'.
  # That tells the software that the model has already been loaded.
  # - vars - A list of objects to keep, typically just 'op'.

  vars <- c(vars, "removeAllExcept")
  keep <- match(x = vars, table = ls(all = T, envir = .GlobalEnv))
  if(any(is.na(keep))){
    modelLoaded <<- FALSE
  }else{
    rm(list=ls(all = T, envir = .GlobalEnv)[-keep], envir = .GlobalEnv)
    modelLoaded <<- TRUE
  }
}
removeAllExcept()

require(PBSmodelling)

options(stringsAsFactors = FALSE)
options(warn = -1)          
source("iscam-gui-globals.r")
if(.OS == "Linux" || .OS == "Darwin"){
  # This stops PBSmodelling from complaining and erroring out in Linux
  #require("Tktable")
  # This changes the windows() function call for Linux to X11()
  windows <- function(...) X11(...)
  # This changes the windows() function call for Linux to system()
  shell   <- function(...) system(...)
}

# iscam-gui sources
source(.UTILITIES_SOURCE)
source(.LOAD_SCENARIOS_SOURCE)
source(.FILE_CONTROL_SOURCE)
source(.REP_PARSER_SOURCE)
source(.FIGURES_SOURCE)
source(.FIGURES_BIOLOGY_SOURCE)
source(.FIGURES_SELEX_SOURCE)
source(.FIGURES_TIMESERIES_SOURCE)
source(.FIGURES_CATCH_SOURCE)

iscam <- function(reloadScenarios      = FALSE,
                  copyModelExecutables = FALSE,
                  silent               = TRUE){
  # iscam()
  # loads model outputs and launches the main iscam-gui GUI.
  # - reloadScenarios TRUE/FALSE - reload the data from all model output files in all scenarios.
  # - copyADMBExecutables TRUE/FALSE copy the admb executable from admb folder to each scenario folder.
  # - silent TRUE/FALSE - show messages on command line

  # Create a global variable which tells the program whether or not to be silent
  # This is the only capitalized, dotted variable not in iscam-gui-globals.r
  .SILENT <<- silent

  graphics.off()  # Destroy graphics window if it exists

  .loadData(reloadScenarios=reloadScenarios,
            copyModelExecutables = copyModelExecutables)

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
    viewOrder             <<- data.frame()
    for(scenario in 1:length(op)){
      viewHeader            <<- rbind(viewHeader,op[[scenario]]$names$scenario)
      viewSensitivityGroups <<- rbind(viewSensitivityGroups,op[[scenario]]$inputs$sensitivityGroup)
      viewColor             <<- rbind(viewColor,op[[scenario]]$inputs$color)
      viewOrder             <<- rbind(viewOrder,op[[scenario]]$inputs$order)
    }
    colnames(viewHeader)            <<- .SCENARIO_LIST_LABEL
    colnames(viewSensitivityGroups) <<- .SENSITIVITY_GROUP_LABEL
    colnames(viewColor)             <<- .PLOT_COLOR_LABEL
    colnames(viewOrder)             <<- .PLOT_ORDER_LABEL
    scenarioHeader <<- cbind(viewHeader,viewSensitivityGroups,viewColor,viewOrder)
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

.doPlots <- function(png=TRUE){
  # png = TRUE means save the plots, FALSE means display on Device.
  graphics.off()
  .PLOT_IS_LIVE <<- FALSE

  val      <- getWinVal()
  plotMCMC <- val$plotMCMC
  ci       <- val$entryConfidence  # Confidence interval
  pType    <- val$viewPlotType
  if(.checkEntries()){
    
    switch(pType,
           # From iscam-gui-figures-timeseries.r
           "sTSSpawningBiomassAllAreas"             = {plotTS(1,png,"SpawningBiomassAllAreas",plotMCMC,ci)},
           "sTSSpawningBiomassByArea"               = {plotTS(2,png,"SpawningBiomassByArea",plotMCMC,ci)},
           "sTSSpawningDepletionAllAreas"           = {plotTS(3,png,"SpawningDepletionAllAreas",plotMCMC,ci)},
           "sTSSpawningDepletionByArea"             = {plotTS(4,png,"SpawningDepletionByArea",plotMCMC,ci)},
           "sTSRecruitmentAllAreas"                 = {plotTS(5,png,"RecruitmentAllAreas",plotMCMC,ci)},
           "sTSRecruitmentByArea"                   = {plotTS(6,png,"RecruitmentByArea",plotMCMC,ci)},
           "sTSIndex"                               = {plotTS(7,png,"Index",plotMCMC,ci)},
           # From iscam-gui-figures-biology.r
           "sBiologyMeanWtAtAge"                    = {plotBiology(1,png,"BiologyMeanWtAtAge",plotMCMC,ci)},
           "sBiologyMaturityAtAge"                  = {plotBiology(2,png,"BiologyMaturityAtAge",plotMCMC,ci)},
           "sBiologyFecundity"                      = {plotBiology(3,png,"BiologyFecundity",plotMCMC,ci)},
           "sBiologyFecundityWeight"                = {plotBiology(4,png,"BiologyFecundityWeight",plotMCMC,ci)},
           "sBiologyFecundityLength"                = {plotBiology(5,png,"BiologyFecundityLength",plotMCMC,ci)},
           "sBiologySpawnOutputLength"              = {plotBiology(6,png,"BiologySpawnOutputLength",plotMCMC,ci)},
           "sBiologyExpectedGrowth"                 = {plotBiology(7,png,"BiologyExpectedGrowth",plotMCMC,ci)},
           "sBiologyTVM"                            = {plotBiology(8,png,"BiologyTVM",plotMCMC,ci)},
           "sBiologyTVGrowthPersp"                  = {plotBiology(9,png,"BiologyTVGrowthPersp",plotMCMC,ci)},
           "sBiologyTVGrowthContour"                = {plotBiology(10,png,"BiologyTVGrowthContour",plotMCMC,ci)},
           # From iscam-gui-figures-selectivities.r
           "sSelexLengthBasedByFleet"               = {plotSelex(1,png,"SelexLengthBasedByFleet",plotMCMC,ci)},
           "sSelexAgeBasedByFleet"                  = {plotSelex(2,png,"SelexAgeBasedByFleet",plotMCMC,ci)},
           "sSelexTVAtLengthSurface"                = {plotSelex(3,png,"SelexTVAtLengthSurface",plotMCMC,ci)},
           "sSelexTVAtLengthContour"                = {plotSelex(4,png,"SelexTVAtLengthContour",plotMCMC,ci)},
           "sSelexTVAtLenthRetentionSurface"        = {plotSelex(5,png,"SelexTVAtLenthRetentionSurface",plotMCMC,ci)},
           "sSelexTVAtLengthRetentionContour"       = {plotSelex(6,png,"SelexTVAtLengthRetentionContour",plotMCMC,ci)},
           "sSelexTVDiscardMortalitySurface"        = {plotSelex(7,png,"SelexTVDiscardMortalitySurface",plotMCMC,ci)},
           "sSelexTVDiscardMortalityContour"        = {plotSelex(8,png,"SelexTVDiscardMortalityContour",plotMCMC,ci)},
           "sSelexRetentionDiscardMortalityEndYear" = {plotSelex(9,png,"SelexRetentionDiscardMortalityEndYear",plotMCMC,ci)},
           "sSelexTVAtAgeSurface"                   = {plotSelex(11,png,"SelexUncertainty",plotMCMC,ci)},
           # From iscam-gui-figures-catch.r
           "sCatchLandings"                         = {plotCatch(1,png,"CatchLandings",plotMCMC,ci)},
           "sCatchLandingsStacked"                  = {plotCatch(2,png,"CatchLandingsStacked",plotMCMC,ci)},
           "sCatchLandingsObsVsExpLandings"         = {plotCatch(3,png,"CatchLandingsObsVsExpLandings",plotMCMC,ci)},
           "sCatchTotal"                            = {plotCatch(4,png,"CatchTotal",plotMCMC,ci)},
           "sCatchTotalStacked"                     = {plotCatch(5,png,"CatchTotalStacked",plotMCMC,ci)},
           "sCatchDiscards"                         = {plotCatch(6,png,"CatchDiscards",plotMCMC,ci)},
           "sCatchDiscardsStacked"                  = {plotCatch(7,png,"CatchDiscardsStacked",plotMCMC,ci)},
           "sCatchDiscardFraction"                  = {plotCatch(8,png,"CatchDiscardFraction",plotMCMC,ci)},
           "sCatchHarvestRate"                      = {plotCatch(9,png,"CatchHarvestRate",plotMCMC,ci)},
           "sCatchLandingsSeasons"                  = {plotCatch(10,png,"CatchLandingsSeasons",plotMCMC,ci)},
           "sCatchLandingsSeasonsStacked"           = {plotCatch(11,png,"CatchLandingsSeasonsStacked",plotMCMC,ci)},
           "sCatchTotalSeasons"                     = {plotCatch(12,png,"CatchTotalSeasons",plotMCMC,ci)},
           "sCatchTotalSeasonsStacked"              = {plotCatch(13,png,"CatchTotalSeasonsStacked",plotMCMC,ci)},
           "sCatchDiscardsSeasons"                  = {plotCatch(14,png,"CatchDiscardsSeasons",plotMCMC,ci)},
           "sCatchDiscardsSeasonsStacked"           = {plotCatch(15,png,"CatchDiscardsSeasonsStacked",plotMCMC,ci)},
           # From iscam-gui-figures.r
           "sParameterPairs"                        = {fig.estimated.params.pairs(scenario=val$entryScenario)},
           "sVariancePartitions"                    = {fig.variance.partitions(scenario=val$entryScenario)},
           # MCMC diagnostics
           "sPriorsVsPosts"                         = {fig.mcmc.priors.vs.posts(exFactor=1.5,showEntirePrior=T,scenario=val$entryScenario)},
           "sMCMCTrace"                             = {fig.mcmc.trace(scenario=val$entryScenario)},
           "sMCMCAutocor"                           = {fig.mcmc.autocor(scenario=val$entryScenario)},
           "sMCMCDensity"                           = {fig.mcmc.density(scenario=val$entryScenario)},
           "sMCMCGeweke"                            = {fig.mcmc.geweke(scenario=val$entryScenario)},
           "sMCMCGelman"                            = {fig.mcmc.gelman(scenario=val$entryScenario)},
           # From iscam-gui-figures-timeseries.r
           "sSensSB"                                = {plotTS(1,png,"SpawningBiomass",plotMCMC,ci,TRUE)},
           "sSensBRatio"                            = {plotTS(3,png,"Depletion",plotMCMC,ci,TRUE)},
           "sSensRecruit"                           = {plotTS(5,png,"Recruitment",plotMCMC,ci,TRUE)},
           "sSensIndex"                             = {plotTS(7,png,"Index",plotMCMC,ci,TRUE)},
           "sSensSPRRatio"                          = {plotTS(7,png,"SPRRatio",plotMCMC,ci,TRUE,btarg=val$entryBtarg,blim=val$entryBlim)},
           "sSensRecruitU"                          = {plotTS(8,png,"RecruitUncertainty",plotMCMC,ci,TRUE)},
           "sSensRecruitDev"                        = {plotTS(9,png,"RecruitmentDev",plotMCMC,ci,TRUE)},
           "sSensIndexLog"                          = {plotTS(12,png,"IndexLog",plotMCMC,ci,TRUE)},
           "sSensDensity"                           = {plotTS(13,png,"Density",plotMCMC,ci,TRUE)},
           # Plot Retrospectives
           "sRetroSB"                               = {plotTS(plotNum=1,retros=TRUE,endyrvec=val$entryEndyr:val$entryStartyr,
                                                              png=png,fileText="RetroSpawningBiomass")},
           "sRetroD"                                = {},
           "sRetroRec"                              = {},
           "sRetroSquid"                            = {plotTS(plotNum=99,retros=TRUE,
                                                              endyrvec = val$entryEndyr:val$entryStartyr,
                                                              cohorts = val$entryFirstCohort:val$entryLastCohort,
                                                              png=png,fileText="RetroSquid")},
           # Plot runtime values returned from ADMB
           "sObjFuncVal"                            = {plotRuntimeStats(1,png,"ObjectiveFunctionValue")},
           "sMaxGrad"                               = {plotRuntimeStats(2,png,"MaximumGradient")},
           "sFuncEvals"                             = {plotRuntimeStats(3,png,"FunctionEvaluations")},
           "sHangCodes"                             = {plotRuntimeStats(4,png,"HangCodes")},
           "sExitCodes"                             = {plotRuntimeStats(5,png,"ExitCodes")},
           {
             # Default
           }
           ) # End switch
  }
  .PLOT_IS_LIVE <<- TRUE
  return(invisible())
}

.subView <- function(png=.PNG, silent = .SILENT){
  act <- getWinAct()
  val <- getWinVal()
  triggerPlot <- TRUE # this triggers plotting upon completion of this call
  # scenarioList is a list of the scenario names (i.e. folder names)
  scenarioList <- as.numeric(rownames(viewHeader))
  # Could sort this but there's no need we are just looking at max and min below
  sensList <- unique(as.numeric(val$scenarioHeader$Group))
  if(length(act)>1){
    act <- act[1]
  }

  # This switch statement represents an 'action' for a button or changing a text field.
  # See iscam-gui-gui-specs.r
  triggerPlot <- FALSE
  switch(act,
         # Change the scenario numnber using three different methods
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
           scenario <- val$entryScenario
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
           if(.runCurrScenario(runMCMC=runMCMC)){
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
             op[[row]]$inputs$order <<- val$scenarioHeader$Order[[row]]
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
         },
         "runAllRetros" = {
           .runAllRetros()
         },
         "saveCurrFigure" = {
           .doPlots(png=TRUE)
         },
         "changeBtarg" = {
           .doPlots(png=png)
         },
         "changeBlim" = {
           .doPlots(png=png)
         },
         {
           triggerPlot <- TRUE
           # Default
         }
         ) # End switch

  # Whichever radio button is selected will now be plotted for the scenario
  if(triggerPlot || (!is.null(dev.list()))){
    .doPlots(png=png)
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
                 warningsText    = op[[scenario]]$inputs$log$numWarnings)
  }else if(loadLogFileSuccess){
    winList <- c(val$winList,
                 mpdTimestamp    = "",
                 mcmcTimestamp   = op[[scenario]]$inputs$log$finishTimes[1],
                 mcevalTimestamp = "MCEval not run",
                 warningsText    = op[[scenario]]$inputs$log$numWarnings)
    if(op[[scenario]]$inputs$log$hasMCeval){
      winList <- c(winList,
                   mpdTimestamp    = "",
                   mcmcTimestamp   = op[[scenario]]$inputs$log$finishTimes[1],
                   mcevalTimestamp = op[[scenario]]$inputs$log$finishTimes[2],
                   warningsText    = op[[scenario]]$inputs$log$numWarnings)
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

.runRetros <- function(scenario=val$entryScenario, silent = .SILENT){
  # Steps:
  # 1. Save the current REP file by copying using file.copy
  # 2. Run the scenario using a system call for MLE retro, 1 for each retro year.
  # 3. Rename each of these runs' REP files to RET* where * is a number.
  # 4. Use file.copy to restore the original REP file

  val <- getWinVal()
  retroYears <- val$entryRetro
  showOutput <- val$showRetroOutput

  # Save the rscripts directory so we can get back to it
  rscriptsDir <- getwd()
  # change to this scenario's directory
  setwd(op[[scenario]][[1]])
  # Save the REP file from the main non-retro run by copying to a backup file
  file.copy("sscam.rep","sscam.backup.rep")

  for(retro in 1:retroYears){
    modelCall <- paste(.EXE_FILE_NAME,"-retro",retro)
    system(modelCall,wait=T,show.output.on.console=showOutput)
    file.copy("sscam.rep",paste("sscam.ret",retro,sep=""),overwrite=T)
  }

  # Reinstantiate the REP file from the main non-retro run
  file.copy("sscam.backup.rep","sscam.rep",overwrite=T)

  setwd(rscriptsDir)
  # Needs fixing...
  .loadScenario(scenario)
  assignGlobals(scenario)
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

