#**********************************************************************************
# iscam-gui-load-scenarios.r
# This file contains the code to load multiple ADMB scenarios into an 'op' list.
# All filenames and foldernames for iscam-gui are set here.
#
# Author            : Chris Grandin
# Development Date  : August 2013 - Present
#
#**********************************************************************************

.loadData <- function(reloadScenarios      = TRUE,
                      copyModelExecutables = FALSE,
                      silent               = .SILENT){
  # .loadData()
  # loads all model output data from all scenarios.
  # - reloadScenarios TRUE/FALSE - reload the data from all model output files in all scenarios.
  # - silent TRUE/FALSE - show messages on command line

  if(!exists("modelLoaded",envir=.GlobalEnv)){
    modelLoaded <<- FALSE
  }
  if(reloadScenarios || !modelLoaded){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Loading data from model output files.")
    op   <<- .loadScenarios(.SCENARIOS_DIR_NAME)
    sens <<- .loadSensitivityGroups(op = op, dired = .SCENARIOS_DIR_NAME)

    modelLoaded <<- T
  }else{
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Using previously loaded data for GUI.  Use ",.MAIN_FUNCTION_CALL,"(T) to reload the Scenarios.\n")
  }
  if(copyModelExecutables){
    .copyExecutableToScenariosDirectories()
  }
}

.loadScenarios <- function(dired = NULL, silent = .SILENT){
  # Return an op list of correct dimensions (prefix op is short for output)
  # dired is a directory name with the output from a model.
  # Also calls a function to cycle through each sensitivity group and call SSsummarize
  # on each so it doesn't have to be called every time a sensitivity plot is drawn.
  #
  # When adding elements, make sure to add also to the data structures section of ../README.md !!

  currFuncName <- getCurrFunc()
  if(!silent){
    cat(.BANNER2)
    cat0(.PROJECT_NAME,"->",currFuncName,"Attempting to load all scenarios.")
  }
  if(is.null(dired)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a directory name to load the scenario from (dired).\n")
    return(NULL)
  }
  dirList <- list.dirs(dired,recursive=FALSE)
  scenarioList <- basename(dirList) # only get subdirectories
  if(length(dirList) == 0){
    return(NULL)
  }
  # Remove instances of the tables and figures directory to avoid infinite recursion
  baseList <- basename(dirList)
  ind <- match(.FIGURES_DIR_NAME,baseList)
  if(!is.na(ind)){
    dirList <- dirList[-ind]
  }
  baseList <- basename(dirList)
  ind <- match(.TABLES_DIR_NAME,baseList)
  if(!is.na(ind)){
    dirList <- dirList[-ind]
  }
  # Now load scenarios for all other directories
  tmpOp <- NULL
  if(length(dirList) > 0){
    tmpOp <- rep(vector("list",length(dirList)))
    # Load main scenarios and retrospectives
    for(scenario in 1:(length(dirList))){
      tmpOp[[scenario]] <- .loadScenario(scenario=scenario, dired = dirList[scenario])
    }
    names(tmpOp) <- basename(dirList)
  }

  if(!silent){
    cat0(.PROJECT_NAME,"->",currFuncName,"Scenarios loaded, global 'op' list object has been populated..")
    cat(.BANNER2)
  }
  return(tmpOp)
}

.loadScenario <- function(scenario, dired = NULL, silent = .SILENT){
  # Load an individual scenario found in the 'dired' directory.
  # Returns a list of a particular structure. See Readme.md for more details
  # This is a recursive function, so any scenario subdirectories will be loaded as well
  # by a call to .loadScenarios() and stored in the sublist $retros

  currFuncName <- getCurrFunc()
  if(is.null(dired)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a dired name to load a scenario.\n")
    return(NULL)
  }
  if(!silent){
    cat(.BANNER)
    cat0(.PROJECT_NAME,"->",currFuncName,"Attempting to load scenario '",dirList[[scenario]],"'")
  }
  # Set up empty variables
  tmp <- NULL
  tmp$names       <- list(scenario          = "",
                          dir               = "",
                          figDir            = "",
                          tableDir          = "",
                          starter           = "",
                          data              = "",
                          control           = "",
                          projection        = "",
                          log               = "",
                          forecast          = "",
                          par               = "",
                          warnings          = "",
                          sensitivityGroup  = "",
                          lastCommandRun    = "")
  tmp$inputs      <- list(sensitivityGroup  = NULL,
                          starter           = NULL,
                          data              = NULL,
                          control           = NULL,
                          projection        = NULL,
                          forecast          = NULL,
                          par               = NULL,
                          numParams         = NULL,
                          objFunVal         = NULL,
                          maxGradient       = NULL,
                          log               = NULL,
                          numWarnings       = NULL,
                          covar             = NULL,
                          lastCommandLine   = NULL)
  tmp$fileSuccess <- list(starter           = FALSE,
                          data              = FALSE,
                          control           = FALSE,
                          projection        = FALSE,
                          mpd               = FALSE,
                          mpdForecast       = FALSE,
                          mcmc              = FALSE,
                          log               = FALSE,
                          par               = FALSE,
                          sensitivityGroup  = FALSE,
                          lastCommandRun    = FALSE)
  tmp$outputs     <- list(mpd               = NULL,
                          mcmc              = NULL,
                          mpdSummary        = NULL,
                          retro             = NULL) # The retrospective plotting code looks at this.
  tmp$names$scenario       <- basename(dired)
  tmp$names$dir            <- dired
  tmp$names$figDir         <- file.path(dired,.FIGURES_DIR_NAME)
  tmp$names$tableDir       <- file.path(dired,.TABLES_DIR_NAME)
  tmp$names$starter        <- file.path(dired,.STARTER_FILE_NAME)
  tmp$names$par            <- file.path(dired,.PAR_FILE_NAME)
  tmp$names$lastCommandRun <- file.path(dired,.LAST_COMMAND_RUN_FILE_NAME)
  tmp$names$log            <- file.path(dired,.LOG_FILE_NAME)

  # Try to load starter file
  tryCatch({
    starterData             <- readStarter(file = tmp$names$starter, verbose=!silent)
    tmp$names$data          <- file.path(dired,starterData[1])
    tmp$names$control       <- file.path(dired,starterData[2])
    tmp$names$projection    <- file.path(dired,starterData[3])
    tmp$fileSuccess$starter <- TRUE
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Problem reading the starter file: '",tmp$names$starter,"'")
    stop()
  }, error = function(err){
    # Do nothing, is is likely not a scenario directory
  })

  # Try to load data file.
  tryCatch({
    tmp$inputs$data      <- readData(file = tmp$names$data, verbose=!silent)
    tmp$fileSuccess$data <- TRUE
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Problem loading data file: '",tmp$names$data,"'")
    stop()
  }, error = function(err){
    # Do nothing, is is likely not a scenario directory
  })

  # Try to load control file.
  tryCatch({
    tmp$inputs$control       <- readControl(file = tmp$names$control, verbose=!silent)
    tmp$fileSuccess$control  <- TRUE
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Problem loading control file: '",tmp$names$control,"'")
    # Ignore errors.  At this point nothing is used yet, just reading it in for fun.
  }, error = function(err){
    # Do nothing, is is likely not a scenario directory
  })

  # Try to load projection file.
  tryCatch({
    tmp$inputs$projection       <- readProjection(file = tmp$names$projection, verbose=!silent)
    tmp$fileSuccess$projection  <- TRUE
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Problem loading projection file: '",tmp$names$projection,"'")
    # Ignore errors.  At this point nothing is used yet, just reading it in for fun.
  }, error = function(err){
    # Do nothing, is is likely not a scenario directory
    cat0(.PROJECT_NAME,"->",currFuncName,"Problem loading projection file: '",tmp$names$projection,"'")
  })

  # Try to load par file.
  tryCatch({
    tmp$inputs$par      <- readLines(tmp$names$par)
    tmp$fileSuccess$par <- TRUE
    convCheck <- tmp$inputs$par[1]
    convCheck <- gsub("[[:alpha:]]+","",convCheck) # remove all letters
    convCheck <- gsub("#","",convCheck)            # remove hash marks
    convCheck <- gsub("=","",convCheck)
    # Note that this might have to be revisited. I don't have an example currently where scientific notation is returned.
    #convCheck <- gsub("-","e-",convCheck)           # replace "e" in scientific notation that may have been removed in the alpha gsub above
    convCheck <- strsplit(convCheck," +",perl=TRUE) # remove spaces and make into a vector of values
    convCheck <- convCheck[[1]][-1]                 # remove first element which is a null string ""
    tmp$inputs$numParams   <- convCheck[1]
    tmp$inputs$objFunValue <- convCheck[2]
    tmp$inputs$maxGradient <- convCheck[3]
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Problem loading par file: '",tmp$names$par,"'")
    # The GUI should be loaded without a par file being present.
    tmp$inputs$numParams   <- ""
    tmp$inputs$objFunValue <- ""
    tmp$inputs$maxGradient <- ""
  }, error = function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"Problem loading par file: '",tmp$names$par,"'")
    # The GUI should be loaded without a par file being present.
    tmp$inputs$numParams   <- ""
    tmp$inputs$objFunValue <- ""
    tmp$inputs$maxGradient <- ""
  })

  # Try to load the scenario's last log file, .LOG_FILE_NAME
  tryCatch({
    tmp$inputs$log <- .loadLogfile(dired = tmp$names$dir)
    tmp$fileSuccess$log <- tmp$inputs$log$isMPD || tmp$inputs$log$isMCMC
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Problem loading log file: '",tmp$names$log,"'")
  }, error = function(err){
    # Ignore errors, since the scenario may not have been run, and therefore no logfile exists.
  })

  # Try to load the scenario's last command run file, .LAST_COMMAND_RUN_FILE_NAME
  tryCatch({
    tmp$inputs$lastCommandRun      <- readTable(tmp$names$lastCommandRun)
    tmp$fileSuccess$lastCommandRun <- TRUE
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Problem loading last command run file: '",tmp$names$lastCommandRun,"'")
  }, error = function(err){
    # Ignore errors, since the scenario may not have been run, and therefore no last command run file exists.
  })

  # Try to load the scenario's scenario info file.
  fnScenarioInfo <-  file.path(dired,.SCENARIO_INFO_FILE_NAME)
  tmp$names$sensitivityGroup       <- fnScenarioInfo
  .createScenarioInfoFile(dired = dired, default = TRUE)
  tryCatch({
    tmp <- .readScenarioInfoFile(dired = dired, sList = tmp)
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,": ",.SCENARIO_INFO_FILE_NAME," file not found, or problem loading it. '",fnScenarioInfo,"'")
    # Create the file since it doesn't exist or is corrupted
    tmp$fileSuccess$sensitivityGroup <- FALSE
   }, error = function(err){
    # Create the file since it doesn't exist or is corrupted
     cat0(.PROJECT_NAME,"->",currFuncName,": ",.SCENARIO_INFO_FILE_NAME," file not found, or problem loading it. '",fnScenarioInfo,"'")
    tmp$fileSuccess$sensitivityGroup <- FALSE
  })

  # Try to load MPD results.
  tryCatch({
    tmp$outputs$mpd <- reptoRlist(file.path(dired,.REPORT_FILE_NAME))
    tmp$fileSuccess$mpd    <- TRUE
    cat0(.PROJECT_NAME,"->",currFuncName,"MPD output loaded for scenario '",dired,"'. (op[[n]]$fileSuccess$mpd)")
  },error=function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"No MPD output found for scenario '",dired,"'. (op[[n]]$fileSuccess$mpd)")
    cat0(.PROJECT_NAME,"->",currFuncName,"  Error message: ", err$message)
  })

  # Try to load MCMC results.  If they don't exist then set a global variable to reflect this
  tryCatch({
    #suppressWarnings(
    #  tmp$outputs$mcmc <- SSgetMCMC(dir=dired,
    #                                verbose=!silent)
    #  )
    #tmp$fileSuccess$mcmc <- TRUE
    cat0(.PROJECT_NAME,"->",currFuncName,"MCMC output loaded for scenario '",dired,"'. (op[[n]]$fileSuccess$mcmc)\n")
  },error=function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"No MCMC output found for scenario '",dired,"'. (op[[n]]$fileSuccess$mcmc)\n")
  })

  if(!silent){
    cat0(.PROJECT_NAME,"->",currFuncName,"Finished loading scenario ",tmp$names$scenario)
    cat0(.BANNER)
  }
  # Make sure the figures and tables directories exist in the main (non-subdirectories), if not, create them
  dirList <- list.dirs(dired, recursive = FALSE)
  # Remove instances of the tables and figures directory to avoid troubles with SSsummarize
  baseList <- basename(dirList)
  ind <- match(.FIGURES_DIR_NAME,baseList)
  if(!is.na(ind)){
    dirList <- dirList[-ind]
  }
  baseList <- basename(dirList)
  ind <- match(.TABLES_DIR_NAME,baseList)
  if(!is.na(ind)){
    dirList <- dirList[-ind]
  }
  # Summarize the retrospectives along with thier base for this scenario
  if(length(dirList) > 0){
    tmp$outputs$retros <- .loadScenarios(dired = dired)
    tmp$outputs$retroSummary <- NULL
    modelList <- NULL
    modelList[[1]] <- tmp$outputs$mpd
    for(retro in 1:length(tmp$outputs$retros)){
      modelList[[retro+1]] <- tmp$outputs$retros[[retro]]$outputs$mpd
    }
    tmp$outputs$retrosSummary <- paste0(currFunc,"NEED TO IMPLEMENT retro loading, was SSsummarize!")
  }

  tmpFdFigures <- file.path(dired,.FIGURES_DIR_NAME)
  tmpFdTables  <- file.path(dired,.TABLES_DIR_NAME)
  dir.create(tmpFdFigures,showWarnings=!silent)
  dir.create(tmpFdTables,showWarnings=!silent)

  return(tmp)
}

.readScenarioInfoFile <- function(dired = NULL, sList = NULL){
  # Reads in the Scenario Info file. Any additions to it need to be accounted for here
  # Any lines beginning with a hash # will be ignored
  currFuncName <- getCurrFunc()
  if(is.null(dired)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a directory name (dired)")
    return(FALSE)
  }
  if(is.null(sList)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a single scenario list to modify.")
    return(FALSE)
  }
  filename <- file.path(dired,.SCENARIO_INFO_FILE_NAME)
  tmp <- read.table(filename)
  sList$inputs$sensitivityGroup <- tmp[1,]
  sList$fileSuccess$sensitivityGroup <- TRUE
  sList$inputs$color <- tmp[2,]
  sList$inputs$order <- tmp[3,] # plotting order
  return(sList)
}

.createScenarioInfoFile <- function(dired = NULL, scenario = NULL, default = FALSE){
  # Writes a scenario info file with default entries in the directory specified by dired,
  # scenario is used for non-default writes only.
  currFuncName <- getCurrFunc()
  if(is.null(dired)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a directory name (dired).")
    return(FALSE)
  }
  val <- getWinVal()
  filename <- file.path(dired,.SCENARIO_INFO_FILE_NAME)
  fileExists <- file.exists(filename)
  writeFile <- FALSE
  if(default){
    # Create a new default file if it does not exist,
    # From the default information (Not the GUI header)
    if(!fileExists){
      tmp <- matrix(list(.SENS_TEXT,
                         .DEFAULT_SENS_GROUP,
                         .PLOT_COLOR_TEXT,
                         .DEFAULT_PLOT_COLOR,
                         .ORDER_TEXT,
                         .DEFAULT_PLOT_ORDER))
      writeFile <- TRUE
    }
  }else{
    # Create the file whether or not it exists, with the information found on
    # the GUI header.
    if(is.null(scenario) ||
       scenario < 1 ||
       scenario > nrow(val$scenarioHeader)){
      cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a scenario number between 1 and ",nrow(val$scenarioHeader))
    }
    tmp <- matrix(list(.SENS_TEXT,
                       val$scenarioHeader$Group[scenario],
                       .PLOT_COLOR_TEXT,
                       val$scenarioHeader$Color[scenario],
                       .ORDER_TEXT,
                       val$scenarioHeader$Order[scenario]
                       ))
    writeFile <- TRUE
  }
  if(writeFile){
    tryCatch({
      write.table(tmp,filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    },warning = function(war){
      cat0(.PROJECT_NAME,"->",currFuncName,"Problem creating file ",filename,".")
      return(FALSE)
    },error = function(err){
      cat0(.PROJECT_NAME,"->",currFuncName,"Problem creating file ",filename,".")
      return(FALSE)
    })
    cat0(.PROJECT_NAME,"->",currFuncName,"Created file ",filename,".")
  }
  return(TRUE)
}

.loadSensitivityGroups <- function(op, dired = NULL, silent = .SILENT){
  # Returns list of sensitivity groups output from SSsummarize
  # found in the 'dired' directory.

  currFuncName <- getCurrFunc()
  if(!silent){
    cat(.BANNER)
    cat0(.PROJECT_NAME,"->",currFuncName,"Attempting to summarize all sensitivity groups.")
  }
  if(is.null(dired)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a directory name (dired).\n")
    return(NULL)
  }
  # Find out how many unique sensitivity groups there are
  uniqueSensGroup <- NULL
  for(scenario in 1:length(op)){
    if(!(op[[scenario]]$inputs$sensitivityGroup %in% uniqueSensGroup)){
      uniqueSensGroup <- c(op[[scenario]]$inputs$sensitivityGroup, uniqueSensGroup)
    }
  }
  uniqueSensGroup <- sort(uniqueSensGroup)
  # Create the sensitivity list that will be returned
  tmp <- rep(vector("list",length(uniqueSensGroup)))
  for(sensGroup in 1:length(uniqueSensGroup)){
    # Get all models in the given sensitivity group
    modelList  <- NULL
    modelNames <- NULL
    iterator   <- 0
    # If any models are an MCMC run, set a variable saying the whole sensitivity group
    # is mcmc, which means it will not be able to be plotted using SSplotComparisons later.
    isMCMC <- FALSE
    for(scenario in 1:length(op)){
      if(op[[scenario]]$inputs$sensitivityGroup == uniqueSensGroup[sensGroup]){
        # Put the mpd output objects into the list
        iterator <- iterator + 1
        modelList[[iterator]]  <- op[[scenario]]$outputs$mpd
        modelNames[[iterator]] <- op[[scenario]]$names$scenario
        if(op[[scenario]]$inputs$log$isMCMC){
          isMCMC <- TRUE
        }
      }
    }
    # Summary is the output from all the model's in a sensitivity group
    #tmp[[sensGroup]]$summary <- SSsummarize(modelList, verbose=!silent)
    tmp[[sensGroup]]$summary <- paste0(currFuncName,"NEED TO IMPLEMENT!")
    # Names are the model names for the legends in the SScomparison plots
    tmp[[sensGroup]]$names   <- modelNames
    tmp[[sensGroup]]$isMCMC  <- isMCMC
  }
  if(!silent){
    cat0(.PROJECT_NAME,"->",currFuncName,"Sensitivity groups loaded, global 'si' list object has been populated..")
    cat(.BANNER)
  }
  return(tmp)
}

.loadLogfile <- function(dired = NULL, filename, silent = .SILENT){
  # Read in the contents of the logfile (.LOG_FILE_NAME)
  #  from the directory dired.
  # set the booleans isMPD, isMCMC, and hasMCeval,
  # Returns tmp, which is a list of logfile outputs and info about it

  if(is.null(dired)){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"You must supply a dired.")
    return(NULL)
  }
  tmp <- NULL
  filename <- file.path(dired,.LOG_FILE_NAME)
  if(file.exists(filename)){
    logData     <- readLines(filename, warn=FALSE)
    finishTimes <- logData[grep("--Finish time:",logData)]
    tmp$finishTimes <- gsub("--Finish time: ","",finishTimes)

    # The warning text was from SS, we could have it in the log output if we wanted for iScam but don't yet.

    #warningText <- logData[grep("warnings: ",logData)]
    #if(length(warningText) > 1){
      # Just take the first instance of the warnings text from the logFile.
    #  warningText <- warningText[1]
    #}
    #tmp$numWarnings <- gsub(".*: ","",warningText)
    #if(tmp$numWarnings == 0){
    #  tmp$numWarnings <- "0"
    #}

    if(length(grep("MCMC",logData)) > 0){
      tmp$isMCMC    <- TRUE
      tmp$isMPD     <- FALSE
    }else{
      tmp$isMCMC    <- FALSE
      tmp$isMPD     <- TRUE
    }

    tmp$hasMCeval   <- FALSE
    # iScam does not have an easy way to check this, yet... So we set it to FALSE for now
    #if(tmp$isMCMC && length(tmp$finishTimes) >= 2){
    #  tmp$hasMCeval <- TRUE
    #}
  }else{
    tmp$finishTimes <- ""
    tmp$numWarnings <- "0"
    tmp$isMPD       <- FALSE
    tmp$isMCMC      <- FALSE
    tmp$hasMCeval   <- FALSE
  }
  return(tmp)
}

.setupCommandLineFromGUI <- function(silent = .SILENT){
  # store the SS command line values from the GUI into the op data structure.
  val <- getWinVal()
  scenario <- val$entryScenario
  op[[scenario]]$inputs$lastCommandLine$maxfn   <<- val$maxfn
  op[[scenario]]$inputs$lastCommandLine$mcmc    <<- val$mcmc
  op[[scenario]]$inputs$lastCommandLine$mcsave  <<- val$mcsave
  op[[scenario]]$inputs$lastCommandLine$mcseed  <<- val$mcseed
  op[[scenario]]$inputs$lastCommandLine$mno     <<- val$mno
  op[[scenario]]$inputs$lastCommandLine$mcscale <<- val$mcscale
  op[[scenario]]$inputs$lastCommandLine$maxph   <<- val$maxph
  op[[scenario]]$inputs$lastCommandLine$mcrd    <<- val$mcrd
  op[[scenario]]$inputs$lastCommandLine$mcprobe <<- val$mcprobe
  op[[scenario]]$inputs$lastCommandLine$gbs     <<- val$gbs
  op[[scenario]]$inputs$lastCommandLine$crit    <<- val$crit
  op[[scenario]]$inputs$lastCommandLine$ams     <<- val$ams
  op[[scenario]]$inputs$lastCommandLine$phase   <<- val$phase
  op[[scenario]]$inputs$lastCommandLine$cbs     <<- val$cbs
  op[[scenario]]$inputs$lastCommandLine$mdl     <<- val$mdl
}

.loadLastCommandRunFile <- function(scenario, silent = .SILENT){
  # Read in the contents of the last command run file (.LAST_COMMAND_RUN_FILE_NAME)
  # set the lastcommands list objects for gui updating.
  # 'scenario' is the scenario number
  # If the file doesn't exist, populate the values with NAs

  if(file.exists(op[[scenario]]$names$lastCommandRun)){
    local({
      load(op[[scenario]]$names$lastCommandRun)
    # commandLine object is stored within lastCommandRun File.
      op[[scenario]]$inputs$lastCommandLine$maxfn   <<- commandLine$maxfn
      op[[scenario]]$inputs$lastCommandLine$mcmc    <<- commandLine$mcmc
      op[[scenario]]$inputs$lastCommandLine$mcsave  <<- commandLine$mcsave
      op[[scenario]]$inputs$lastCommandLine$mcseed  <<- commandLine$mcseed
      op[[scenario]]$inputs$lastCommandLine$mno     <<- commandLine$mno
      op[[scenario]]$inputs$lastCommandLine$mcscale <<- commandLine$mcscale
      op[[scenario]]$inputs$lastCommandLine$maxph   <<- commandLine$maxph
      op[[scenario]]$inputs$lastCommandLine$mcrd    <<- commandLine$mcrd
      op[[scenario]]$inputs$lastCommandLine$mcprobe <<- commandLine$mcprobe
      op[[scenario]]$inputs$lastCommandLine$gbs     <<- commandLine$gbs
      op[[scenario]]$inputs$lastCommandLine$crit    <<- commandLine$crit
      op[[scenario]]$inputs$lastCommandLine$ams     <<- commandLine$ams
      op[[scenario]]$inputs$lastCommandLine$phase   <<- commandLine$phase
      op[[scenario]]$inputs$lastCommandLine$cbs     <<- commandLine$cbs
      op[[scenario]]$inputs$lastCommandLine$mdl     <<- commandLine$mdl
    })
  }else{
    # There was no last run, or file was deleted
    op[[scenario]]$inputs$lastCommandLine$maxfn    <<- NA
    op[[scenario]]$inputs$lastCommandLine$mcmc     <<- NA
    op[[scenario]]$inputs$lastCommandLine$mcsave   <<- NA
    op[[scenario]]$inputs$lastCommandLine$mcseed   <<- NA
    op[[scenario]]$inputs$lastCommandLine$mno      <<- NA
    op[[scenario]]$inputs$lastCommandLine$mcscale  <<- NA
    op[[scenario]]$inputs$lastCommandLine$maxph    <<- NA
    op[[scenario]]$inputs$lastCommandLine$mcrb     <<- NA
    op[[scenario]]$inputs$lastCommandLine$mcprobe  <<- NA
    op[[scenario]]$inputs$lastCommandLine$gbs      <<- NA
    op[[scenario]]$inputs$lastCommandLine$crit     <<- NA
    op[[scenario]]$inputs$lastCommandLine$ams      <<- NA
    op[[scenario]]$inputs$lastCommandLine$phase    <<- NA
    op[[scenario]]$inputs$lastCommandLine$cbs      <<- NA
    op[[scenario]]$inputs$lastCommandLine$mdl      <<- NA
    return(FALSE)
  }
  return(TRUE)
}

.writeLastCommandRunFile <- function(scenario, silent = .SILENT){
  # Write the last command run file (.LAST_COMMAND_RUN_FILE_NAME)
  # retrieve the values from the gui
  # 'scenario' is the scenario number

  val <- getWinVal()
  commandLine <- list(val$maxfn,
                      val$mcmc,
                      val$mcsave,
                      val$mcseed,
                      val$mno,
                      val$mcscale,
                      val$maxph,
                      val$mcrd,
                      val$mcprobe,
                      val$gbs,
                      val$crit,
                      val$ams,
                      val$phase,
                      val$cbs,
                      val$mdl)
  names(commandLine) <- c("maxfn",
                          "mcmc",
                          "mcsave",
                          "mcseed",
                          "mno",
                          "mcscale",
                          "maxph",
                          "mcrd",
                          "mcprobe",
                          "gbs",
                          "crit",
                          "ams",
                          "phase",
                          "cbs",
                          "mdl")
  save(commandLine, file=op[[scenario]]$names$lastCommandRun)
}

readStarter <- function(file = NULL, verbose = FALSE){
  # Read the starter file into a vector, which for iscam is just a file with
  # the data file, control file, and projection file names in that order
  return(readLines(file))
}

readData <- function(file = NULL, verbose = FALSE){
  # Read in the datafile given by 'file'
  return(readLines(file))
}

readControl <- function(file = NULL, verbose = FALSE){
  # Read in the control file given by 'file'
  return(readLines(file))
}

readProjection <- function(file = NULL, verbose = FALSE){
  # Read in the projection file given by 'file'
  return(readLines(file))
}
