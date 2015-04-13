#**********************************************************************************
# iscam-gui-load-catchdata.r
# This file contains the code to load catch data from the directory 'Catchdata'
#
# Author            : Chris Grandin
# Development Date  : April 2015 - Present
#**********************************************************************************

.loadCatchdata <- function(){
  # Load the catch data in the file pointed to by the global .CATCHDATA_FILE_NAME
  # Note that the data must have an object caleld 'catch'
  load(.CATCHDATA_FILE_NAME)
  # The following assumes that the newly loaded data is the first
  # object, or only object in this (function's) environment.
  cat0(.PROJECT_NAME,"->",getCurrFunc(),"Catch data loaded from the file '",.CATCHDATA_FILE_NAME,"'")
  cat0(.PROJECT_NAME,"->",getCurrFunc()," and stored in global object 'catch'.")
  return(catch)
}
