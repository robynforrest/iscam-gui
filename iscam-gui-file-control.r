
#**********************************************************************************
# ss-explore-file-control.r
# This file contains:
# 1. Functions for creating/editing dat, control, and starter files for the stock of your choice.
# 2. Functions for copying these files to the execution directory
# 3. Functions for copying the outputs from the admb runs back to the scenario directories
# 4. Function to copy model executable to each Scenarios' folder
#
# The data structure used is an op, which is a list of lists, see loadScenarios.r for
# details on this structure. This file assumes that an object called 'op'
# exists and is a valid op.
#
# Assumes that the tcl/tk window is open and ready to be read from/written to
# using PBSmodelling commands such as setWinVal() and getWinVal().  Also assumes
# that the variable fdScenario is in the .GlobalEnv workspace and that it points to the
# current Scenario.
# Assumes ss-explore-load-scenarios.r has been sourced. It instantiates the path and file name variables.
#
# Author            : Chris Grandin
# Development Date  : September 2013
#
#**********************************************************************************

.copyExecutableToScenarioDirectory <- function(scenario, silent = .SILENT){
# Copies the model executable from the execution directory to
# the given scenario folder
  src <- .SS_EXE_FILE_NAME_FULL_PATH
  des <- file.path(op[[scenario]]$names$dir,.SS_EXE_FILE_NAME)
  file.copy(src, des, overwrite = TRUE)
  if(!silent){
    cat("Copied model executable from '",src,"' to '",des,"'\n\n",sep="")
  }
}

.copyExecutableToScenariosDirectories <- function(silent = .SILENT){
# Copies the model executable from the execution directory to
# all scenarios folders.
  for(scenario in 1:length(dirList)){
    .copyExecutableToScenarioDirectory(scenario)
  }
}

.deleteMPDOutputs <- function(scenario, silent = .SILENT){
  # delete all MPD files output by SS3.  Keep MCMC output files if they exist.
  # TODO: Need to add retrospective RET* files to deletion list if they exist.
  mpdOutputsFullPath <- file.path(op[[scenario]]$names$dir, .MPD_OUTPUT_FILES)
  if(!any(file.exists(mpdOutputsFullPath))){
      cat(.PROJECT_NAME,"->.deleteMPDOutputs: The model in '",op[[scenario]]$names$dir,
          "' has not been run (no output files exist).\n",sep="")
    return(TRUE)
  }else if(any(file.exists(mpdOutputsFullPath)) &&
     getYes(paste("Warning, you are about to delete all outputs for the '",op[[scenario]]$names$scenario,
                  "' scenario.  Continue?",sep=""),title="Delete Files?",icon="question")){
    unlink(mpdOutputsFullPath)
    if(!silent){
      cat(.PROJECT_NAME,"->.deleteMPDOutputs: Deleted MPD outputs from '",op[[scenario]]$names$dir,"'.\n",sep="")
    }
    return(TRUE)
  }
  return(FALSE)
}

.editFile <- function(scenario, type, silent = .SILENT){
  # type is one of 1, 2, 3, 4, 5, 6 where:
  # 1 = Control file
  # 2 = Data file
  # 3 = Starter file
  # 4 = Command line output
  # 5 = Forecast file
  # 6 = Warning file
  # 7 = PAR file
  if(type==1){
    editCall <- paste(.EDITOR,op[[scenario]]$names$control)
    shell(editCall, wait=F)
  }
  if(type==2){
    editCall <- paste(.EDITOR,op[[scenario]]$names$data)
    shell(editCall, wait=F)
  }
  if(type==3){
    editCall <- paste(.EDITOR,op[[scenario]]$names$starter)
    shell(editCall, wait=F)
  }
  if(type==4){
    if(file.exists(op[[scenario]]$names$log)){
      editCall <- paste(.EDITOR,op[[scenario]]$names$log)
      shell(editCall, wait=F)
    }else{
      cat(.PROJECT_NAME,"->.editFile: '",op[[scenario]]$names$log,"' does not exist.\n",
          "You must run the model from within the GUI to create this file.\n")
    }
  }
  if(type==5){
    editCall <- paste(.EDITOR,op[[scenario]]$names$forecast)
    shell(editCall, wait=F)
  }
  if(type==6){
    if(file.exists(op[[scenario]]$names$log)){
      editCall <- paste(.EDITOR,op[[scenario]]$names$warnings)
      shell(editCall, wait=F)
    }else{
      cat(.PROJECT_NAME,"->.editFile: '",op[[scenario]]$names$warnings,"' does not exist.\n",
          "You must run the model from within the GUI to create this file.\n")
    }
  }
  if(type==7){
    if(file.exists(op[[scenario]]$names$par)){
      editCall <- paste(.EDITOR,op[[scenario]]$names$par)
      shell(editCall, wait=F)
    }else{
      cat(.PROJECT_NAME,"->.editFile: '",op[[scenario]]$names$par,"' does not exist.\n",
          "You must run the model from within the GUI to create this file.\n")
    }
  }

}
