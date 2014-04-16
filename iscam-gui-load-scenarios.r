#**********************************************************************************
# iscam-gui-load-scenarios.r
# This file contains the code to load multiple ADMB scenarios into an 'op' list.
# All filenames and foldernames for iscam-gui are set here.
#
# Author            : Chris Grandin
# Development Date  : August 2013 - Present
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
    sens <<- .loadSensitivityGroups(op = op)
    modelLoaded <<- TRUE
  }else{
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Using previously loaded data for GUI.  Use ",.MAIN_FUNCTION_CALL,"(TRUE) to reload the Scenarios.\n")
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
                          par               = "",
                          mcmc              = "",
                          mcmcsbt           = "",
                          mcmcrt            = "",
                          warnings          = "",
                          sensitivityGroup  = "",
                          lastCommandRun    = "")
  tmp$inputs      <- list(sensitivityGroup  = NULL,
                          starter           = NULL,
                          data              = NULL,
                          control           = NULL,
                          projection        = NULL,
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
                          mcmcsbt           = FALSE,
                          mcmcrt            = FALSE,
                          log               = FALSE,
                          par               = FALSE,
                          sensitivityGroup  = FALSE,
                          lastCommandRun    = FALSE)
  tmp$outputs     <- list(mpd               = NULL,
                          mcmc              = NULL,
                          par               = NULL,
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
    cat0(.PROJECT_NAME,"->",currFuncName,"Warning - problem loading the starter file: '",tmp$names$starter,"'")
    cat0(.PROJECT_NAME,"->",currFuncName,war$message)
    stop()
  }, error = function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"Error - problem loading the starter file: '",tmp$names$starter,"'")
    cat0(.PROJECT_NAME,"->",currFuncName,err$message)
    # Do nothing, is is likely not a scenario directory
  })

  # Try to load data file.
  tryCatch({
    tmp$inputs$data      <- readData(file = tmp$names$data, verbose=!silent)
    tmp$fileSuccess$data <- TRUE 
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Warning - problem loading data file: '",tmp$names$data,"'")
    cat0(.PROJECT_NAME,"->",currFuncName,war$message)
    stop()
  }, error = function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"Error - problem loading data file: '",tmp$names$data,"'")
    cat0(.PROJECT_NAME,"->",currFuncName,war$message)
    # Do nothing, is is likely not a scenario directory
  })

  # Try to load control file.
  tryCatch({
    tmp$inputs$control <- readControl(file    = tmp$names$control,
                                      ngears  = tmp$inputs$data$ngear,
                                      nagears = tmp$inputs$data$nagears,
                                      verbose =!silent)
    tmp$fileSuccess$control  <- TRUE
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Warning - problem loading control file: '",tmp$names$control,"'")
    cat0(.PROJECT_NAME,"->",currFuncName,war$message)
    stop()
  }, error = function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"Warning - problem loading control file: '",tmp$names$control,"'")
    cat0(.PROJECT_NAME,"->",currFuncName,err$message)
    # Do nothing, is is likely not a scenario directory
  })

  # Try to load projection file.
  tryCatch({
    tmp$inputs$projection       <- readProjection(file = tmp$names$projection, verbose=!silent)
    tmp$fileSuccess$projection  <- TRUE
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Warning - problem loading projection file: '",tmp$names$projection,"'")
    cat0(.PROJECT_NAME,"->",currFuncName,war$message)
    stop()
  }, error = function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"Error - problem loading projection file: '",tmp$names$projection,"'")
    cat0(.PROJECT_NAME,"->",currFuncName,err$message)
  })

  # Try to load par file.
  tryCatch({
    tmp$outputs$par     <- readPar(tmp$names$par)
    tmp$fileSuccess$par <- TRUE
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Warning - problem loading par file: '",tmp$names$par,"'")
    # The GUI should be loaded without a par file being present.
    tmp$outputs$par$numParams   <- ""
    tmp$outputs$par$objFunValue <- ""
    tmp$outputs$par$maxGradient <- ""
  }, error = function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"Error - problem loading par file: '",tmp$names$par,"'")
    # The GUI should be loaded without a par file being present.
    tmp$outputs$par$numParams   <- ""
    tmp$outputs$par$objFunValue <- ""
    tmp$outputs$par$maxGradient <- ""
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
    tmp$outputs$mcmc <- readMCMC(dired = dired, verbose=!silent)
    tmp$fileSuccess$mcmc <- TRUE
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

.loadSensitivityGroups <- function(op, silent = .SILENT){
  # Returns vector of numbers which relate to the models in the 'op' list,
  # so that plotting code can later access the output and plot them together
  # found in the 'dired' directory.

  currFuncName <- getCurrFunc()
  if(!silent){
    cat(.BANNER)
    cat0(.PROJECT_NAME,"->",currFuncName,"Attempting to summarize all sensitivity groups.")
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
  tmp <- vector("list", length = max(uniqueSensGroup))
  for(sensGroup in 1:length(uniqueSensGroup)){
    iterator   <- 0
    for(scenario in 1:length(op)){
      if(op[[scenario]]$inputs$sensitivityGroup == uniqueSensGroup[sensGroup]){
        iterator <- iterator + 1
        tmp[[uniqueSensGroup[sensGroup]]][iterator] <- scenario
      }
    }
  }
  if(!silent){
    cat0(.PROJECT_NAME,"->",currFuncName,"Sensitivity groups loaded, global 'sens' list object has been populated..")
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

    # The '2' in the following is because the sentence appears twice in the output.
    hessWarn <- logData[grep("Hessian does not appear to be positive definite",logData)][2]
    if(!is.na(hessWarn)){
      tmp$hessianWarning <- "Non-positive-definite Hessian"
    }else{
      tmp$hessianWarning <- ""
    }

    if(length(grep("mcmc",logData)) > 0){
      tmp$isMCMC    <- TRUE
      tmp$isMPD     <- FALSE
    }else{
      tmp$isMCMC    <- FALSE
      tmp$isMPD     <- TRUE
    }
    tmp$hasMCeval   <- FALSE
    if(length(grep("mceval",logData)) > 0){
      tmp$hasMCeval   <- TRUE
    }
  }else{
    tmp$finishTimes  <- ""
    tmp$warningsText <- ""
    tmp$isMPD        <- FALSE
    tmp$isMCMC       <- FALSE
    tmp$hasMCeval    <- FALSE
  }
  return(tmp)
}

.setupCommandLineFromGUI <- function(silent = .SILENT){
  # store the command line values from the GUI into the op data structure.
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
  return(readLines(file, warn=FALSE))
}

readData <- function(file = NULL, verbose = FALSE){
  # Read in the iscam datafile given by 'file'
  # Parses the file into its constituent parts
  # And returns a list of the contents

  data <- readLines(file, warn=FALSE)

  # Remove any empty lines
  data <- data[data != ""]

  # remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+","",data)

  # Get the element numbers which start with #.
  dat <- grep("^#.*",data)
  # remove the lines that start with #.
  dat <- data[-dat]

  # remove comments which come at the end of a line
  dat <- gsub("#.*","",dat)

  # remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+","",dat)
  dat <- gsub("[[:blank:]]+$","",dat)

  # Now we have a nice bunch of string elements which are the inputs for iscam.
  # Here we parse them into a list structure
  # This is dependent on the current format of the DAT file and needs to
  # be updated whenever the DAT file changes format
  tmp <- list()
  ind <- 0
  tmp$narea  <- as.numeric(dat[ind <- ind + 1])
  tmp$ngroup <- as.numeric(dat[ind <- ind + 1])
  tmp$nsex   <- as.numeric(dat[ind <- ind + 1])
  tmp$syr    <- as.numeric(dat[ind <- ind + 1])
  tmp$nyr    <- as.numeric(dat[ind <- ind + 1])
  tmp$sage   <- as.numeric(dat[ind <- ind + 1])
  tmp$nage   <- as.numeric(dat[ind <- ind + 1])
  tmp$ngear  <- as.numeric(dat[ind <- ind + 1])
  # Gear allocation
  tmp$alloc  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  # Age-schedule and population parameters
  tmp$linf   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$k      <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$to     <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$lwscal <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$lwpow  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$age50  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$sd50   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$usemat <- as.numeric(dat[ind <- ind + 1])
  tmp$matvec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  # Catch data
  tmp$nctobs <- as.numeric(dat[ind <- ind + 1])
  tmp$catch  <- matrix(NA, nrow = tmp$nctobs, ncol = 7)
  for(row in 1:tmp$nctobs){
    tmp$catch[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  colnames(tmp$catch) <- c("year","gear","area","group","sex","type","value")
  # Abundance indices are a ragged object and are stored as a list of matrices
  tmp$nit     <- as.numeric(dat[ind <- ind + 1])
  tmp$nitnobs <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmpsurvtype <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  #nrows <- sum(tmp$nitnobs)
  tmp$indices <- list()
  for(index in 1:tmp$nit){
    nrows <- tmp$nitnobs[index]
    ncols <- 8
    tmp$indices[[index]] <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$indices[[index]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$indices[[index]]) <- c("iyr","it","gear","area","group","sex","wt","timing")
  }
  # Age composition data are a ragged object and are stored as a list of matrices
  tmp$nagears     <- as.numeric(dat[ind <- ind + 1])
  tmp$nagearsvec  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$nagearssage <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$nagearsnage <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$eff         <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$agecomps    <- NULL
  # one list element for each gear (tmp$nagears)
  if (tmp$nagearsvec > 0) {   #but, only if age composition data exist (i.e., 1 or more rows or data)
     tmp$agecomps    <- list()
     for(gear in 1:tmp$nagears){
       nrows <- tmp$nagearsvec[gear]
       ncols <- tmp$nagearsnage[gear] - tmp$nagearssage[gear] + 6 # 5 of the 6 here is for the header               columns
       tmp$agecomps[[gear]] <- matrix(NA, nrow = nrows, ncol = ncols)
       for(row in 1:nrows){
         tmp$agecomps[[gear]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
       }
       colnames(tmp$agecomps[[gear]]) <- c("year","gear","area","group","sex",tmp$nagearssage[gear]:tmp$nagearsnage[gear])
     }
  }
  # Empirical weight-at-age data
  tmp$nwttab <- as.numeric(dat[ind <- ind + 1])
  tmp$nwtobs <- as.numeric(dat[ind <- ind + 1])
  tmp$waa <- NULL

  if(tmp$nwtobs > 0){
    # Parse the weight-at-age data
    nrows       <- tmp$nwtobs
    ncols       <- tmp$nage - tmp$sage + 6
    tmp$waa <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$waa[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$indices) <- c("year","gear","area","group","sex",tmp$sage:tmp$nage)
  }
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  return(tmp)
}

readControl <- function(file = NULL, ngears = NULL, nagears = NULL, verbose = FALSE){
  # Read in the iscam control file given by 'file'
  # Parses the file into its constituent parts
  # And returns a list of the contents
  # ngears is the total number of gears in the datafile
  # magears in the number of gears with age composition information in the datafile

  currFuncName <- getCurrFunc()
  if(is.null(ngears)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply the total number of gears (ngears). Returning NULL.")
    return(NULL)
  }
  if(is.null(nagears)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply the number of gears with age composition (nagears). Returning NULL.")
    return(NULL)
  }

  data <- readLines(file, warn=FALSE)

  # Remove any empty lines
  data <- data[data != ""]

  # remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+","",data)

  # Get the element numbers which start with #.
  dat <- grep("^#.*",data)
  # remove the lines that start with #.
  dat <- data[-dat]

  # Save the parameter names, since they are comments and will be deleted in
  # subsequent steps
  # To get the npar, remove any comments and preceeding and trailing whitespace for it
  dat1 <- gsub("#.*","",dat[1])
  dat1 <- gsub("^[[:blank:]]+","",dat1)
  dat1 <- gsub("[[:blank:]]+$","",dat1)
  npar <- as.numeric(dat1)
  paramNames <- vector()
  # Lazy matching with # so that the first instance matches, not any other
  pattern <- "^.*#[[:blank:]]*([[:alnum:]]+_*[[:alnum:]]*) +.*"
  for(paramName in 1:npar){
    # Each parameter line in dat which starts at index 2,
    # retrieve the parameter name for that line
    paramNames[paramName] <- sub(pattern,"\\1",dat[paramName+1])
  }

  # Now that parameter names are stored, parse the file.
  # remove comments which come at the end of a line
  dat <- gsub("#.*","",dat)

  # remove preceeding and trailing whitespace, but not between whitespace
  dat <- gsub("^[[:blank:]]+","",dat)
  dat <- gsub("[[:blank:]]+$","",dat)

  # Now we have a nice bunch of string elements which are the inputs for iscam.
  # Here we parse them into a list structure
  # This is dependent on the current format of the DAT file and needs to
  # be updated whenever the DAT file changes format
  tmp <- list()
  ind <- 0
  tmp$npar <- as.numeric(dat[ind <- ind + 1])
  tmp$param <- matrix(NA, nrow = tmp$npar, ncol = 7)
  for(param in 1:tmp$npar){
    tmp$param[param,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  colnames(tmp$param) <- c("ival","lb","ub","phz","prior","p1","p2")
  rownames(tmp$param) <- paramNames # Retreived at the beginning of this function

  # Age and size composition control parameters and likelihood types
  nrows <- 8
  ncols <- nagears
  tmp$as <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$as[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  # Rownames here are hardwired, so if you add a new row you must add a name for it here
  rownames(tmp$as) <- c("gearind","likelihoodtype","minprop","comprenorm","logagetau2phase",
                        "phi1phase","phi2phase","degfreephase")

  ind <- ind + 1 # Ignore the int check value

  # Selectivity parameters for all gears
  nrows <- 10
  ncols <- ngears
  tmp$sel <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$sel[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  # Rownames here are hardwired, so if you add a new row you must add a name for it here
  rownames(tmp$sel) <- c("iseltype","agelen50log","std50log","nagenodes","nyearnodes",
                         "estphase","penwt2nddiff","penwtdome","penwttvs","nselblocks")

  # Start year for time blocks, one for each gear
  tmp$syrtimeblock <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])

  # Priors for survey Q, one column for each survey
  tmp$nits <- as.numeric(dat[ind <- ind + 1])
  nrows <- 3
  ncols <- tmp$nits
  tmp$survq <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$survq[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  # Rownames here are hardwired, so if you add a new row you must add a name for it here
  rownames(tmp$survq) <- c("priortype","priormeanlog","priorsd")

  nrows <- 15
  tmp$misc <- matrix(NA, nrow = nrows, ncol = 1)
  for(row in 1:nrows){
    tmp$misc[row,1] <- as.numeric(dat[ind <- ind + 1])
  }
  # Rowames here are hardwired, so if you add a new row you must add a name for it here
  rownames(tmp$misc) <- c("verbose","rectype","sdobscatchfirstphase","sdobscatchlastphase",
                          "unfishedfirstyear","minpropage","meanF","sdmeanFfirstphase",
                          "sdmeanFlastphase","mdevphase","sdmdev","mnumestnodes",
                          "fracZpriorspawn","agecompliketype","IFDdist")
  tmp$eof <- as.numeric(dat[ind <- ind + 1])  
  return(tmp)
}

readProjection <- function(file = NULL, verbose = FALSE){
  # Read in the projection file given by 'file'
  # Parses the file into its constituent parts
  # And returns a list of the contents

  data <- readLines(file, warn=FALSE)

  # Remove any empty lines
  data <- data[data != ""]

  # remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+","",data)

  # Get the element numbers which start with #.
  dat <- grep("^#.*",data)
  # remove the lines that start with #.
  dat <- data[-dat]

  # remove comments which come at the end of a line
  dat <- gsub("#.*","",dat)

  # remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+","",dat)
  dat <- gsub("[[:blank:]]+$","",dat)

  # Now we have a nice bunch of string elements which are the inputs for iscam.
  # Here we parse them into a list structure
  # This is dependent on the current format of the DAT file and needs to
  # be updated whenever the DAT file changes format
  tmp <- list()
  ind <- 0

  # Get the TAC values
  tmp$ntac  <- as.numeric(dat[ind <- ind + 1])
  tmp$tacvec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])

  # Get the control options vector
  tmp$ncntrloptions <- as.numeric(dat[ind <- ind + 1])
  nrows <- tmp$ncntrloptions
  ncols <- 1
  tmp$cntrloptions  <- matrix (NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$cntrloptions[row,1] <- as.numeric(dat[ind <- ind + 1])
  }
  # Rownames here are hardwired, so if you add a new row you must add a name for it here
  rownames(tmp$cntrloptions) <- c("syrmeanm","nyrmeanm",
                                  "syrmeanfecwtageproj","nyrmeanfecwtageproj",
                                  "syrmeanrecproj","nyrmeanrecproj")
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  return(tmp)
}

readPar <- function(file = NULL, verbose = FALSE){
  # Read in the parameter estimates file given by 'file'
  # Parses the file into its constituent parts
  # And returns a list of the contents

  data <- readLines(file, warn=FALSE)
  tmp <- list()
  ind <- 0

  # Remove preceeding #
  convCheck <- gsub("^#[[:blank:]]*","",data[1])
  # Remove all letters
  convCheck <- gsub("[[:alpha:]]+","",convCheck)
  # Remove the equals signs
  convCheck <- gsub("=","",convCheck)
  # Remove all preceeding and trailing whitespace
  convCheck <- gsub("^[[:blank:]]+","",convCheck)
  convCheck <- gsub("[[:blank:]]+$","",convCheck)
  # Get the values, round is used to force non-scientific notation
  convCheck <- as.numeric(strsplit(convCheck,"[[:blank:]]+")[[1]])

  # Note that this might have to be revisited. I don't have an example currently where scientific notation is returned.
  #convCheck <- gsub("-","e-",convCheck) # replace "e" in scientific notation that may have been removed in the alpha gsub above    convCheck <- strsplit(convCheck," +",perl=TRUE) # remove spaces and make into a vector of values
  # The following values are saved for appending to the tmp list later
  numParams   <- convCheck[1]
  objFunValue <- as.numeric(sprintf("%1.3f",convCheck[2]))
  maxGradient <- as.numeric(sprintf("%1.9f",convCheck[3]))

  # Remove the first line from the par data since we already parsed it and saved the values
  data <- data[-1]

  # At this point, every odd line is a comment and every even line is the value.
  # Parse the names from the odd lines (oddData) and parse the
  # values from the even lines (evenData)
  oddElem <- seq(1,length(data),2)
  evenElem <- seq(2,length(data),2)
  oddData <- data[oddElem]
  evenData <- data[evenElem]

  # remove preceeding and trailing whitespace if it exists from both names and values
  names <- gsub("^[[:blank:]]+","",oddData)
  names <- gsub("[[:blank:]]+$","",names)
  values <- gsub("^[[:blank:]]+","",evenData)
  values <- gsub("[[:blank:]]+$","",values)

  # Remove the preceeding # and whitespace and the trailing : from the names
  pattern <- "^#[[:blank:]]*(.*)[[:blank:]]*:"
  names <- sub(pattern,"\\1",names)

  # Remove any square brackets from the names
  names <- gsub("\\[|\\]","",names)

  dataLength <- length(names)
  for(item in 1:(dataLength)){
    tmp[[item]] <- as.numeric(strsplit(values[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }

  names(tmp) <- names
  tmp$numParams <- numParams
  tmp$objFunValue <- objFunValue
  tmp$maxGradient <- maxGradient
  return(tmp)
}

readMCMC <- function(dired = NULL, verbose = TRUE){
  # Read in the MCMC results from an iscam model run found in the directory dired.
  # Returns a list of the mcmc outputs, or NULL if there was a problem or
  # There are no MCMC outputs

  if(is.null(dired)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a directory name (dired). Returning NULL.")
    return(NULL)
  }
  mcmcfn    <- file.path(dired,.MCMC_FILE_NAME)
  mcmcsbtfn <- file.path(dired,.MCMC_BIOMASS_FILE_NAME)
  mcmcrtfn  <- file.path(dired,.MCMC_RECRUITMENT_FILE_NAME)

  tmp        <- list()
  tmp$params <- read.csv(mcmcfn)
  sbt        <- read.csv(mcmcsbtfn)
  tmp$sbt    <- extractGroupMatrices(sbt, prefix = "sbt")
  rt         <- read.csv(mcmcrtfn)
  tmp$rt     <- extractGroupMatrices(rt, prefix = "rt")
  return(tmp)
}

extractGroupMatrices <- function(data = NULL, prefix = NULL){
  # Extract the data frame given (data) by unflattening into t alist of matrices
  # by group. The group number is located in the names of the columns of the
  # data frame in this format: "prefix[groupnum]_year" where [groupnum] is one
  # or more digits representing the group number and prefix is the string
  # given as an argument to the function.
  # Returns a list of matrices, one element per group.

  currFuncName <- getCurrFunc()
  if(is.null(data) || is.null(prefix)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must give two arguments (data & prefix). Returning NULL.")
    return(NULL)
  }
  tmp <- list()

  names <- names(data)
  pattern <- paste0(prefix,"([[:digit:]]+)_[[:digit:]]+")
  groups  <- sub(pattern,"\\1",names)
  uniqueGroups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(uniqueGroups))
  # This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(uniqueGroups)){
    # Get all the column names (groupNames) for this group by making a specific pattern for it
    groupPattern <- paste0(prefix,group,"_[[:digit:]]+")
    groupNames   <- names[grep(groupPattern, names)]
    # Remove the group number in the name, as it is not needed anymore
    pattern      <- paste0(prefix,"[[:digit:]]+_([[:digit:]]+)")
    groupNames   <- sub(pattern,"\\1",groupNames)

    # Now, the data must be extracted
    # Get the column numbers that this group are included in
    dat <- data[,grep(groupPattern, names)]
    colnames(dat) <- groupNames
    tmp[[group]]  <- dat
  }
  return(tmp)
}
