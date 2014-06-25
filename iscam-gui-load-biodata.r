#**********************************************************************************
# iscam-gui-load-biodata.r
# This file contains the code to load biological data in the PBSModelling format,
#  and to extract length, weight, and age data and run ADMB models to fit LW and VonB.
#
# Author            : Chris Grandin
# Development Date  : June 2014 - Present
#**********************************************************************************

.runBioModel <- function(model, ages, areas, splitSex, surveys,
                         multLen = 1, multWt = 1){
  # Run a model after extracting the given ages and areas,
  # model = 1 is length/weight model
  # model = 2 is a vonB model
  # model = 3 is a maturity/age model
  # ages is a vector of ages for query
  # areas is a vector of areas i.e. 3C, 3D, etc
  # and split the sexes if splitSex=TRUE.
  # multLen and multWt are multipliers for the length and weight for unit conversion.
  # surveys is a vector of codes (key number) from the global surveyKeys declared at the bottom of this file.
  currDir <- getwd()
  dir <- .BIODATA_DIR_NAME
  ## switch(model,
  ##        1 = {
  ##          exe <- file.path(dir, .LW_EXE_FILE_NAME)
  ##          if(splitSex){
  ##            for(sex in 1:2){
  ##              createLengthWeightDatafile(areas, sex, survey, multLen, multWt)
  ##            }
  ##          }
  ##        },
  ##        2 = {
  ##        },
  ##        3 = {
  ##        },
  ##        {
  ##          # default
  ##        }
  ## }
  if(!exists("bio", envir = .GlobalEnv)){
    bio <<- NULL
  }
  if(model == 1){
    # Check that executable exists
    exe <- file.path(dir, .LW_EXE_FILE_NAME)
    if(!file.exists(exe)){
      cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - '",exe,"' does not exist. Compile it and try again.")
      return(NULL)
    }
    if(splitSex){
      for(sex in 1:2){
        if(!createLengthWeightDatafile(areas, sex, surveys, multLen, multWt)){
          return(NULL)
        }
        # Change to the directory to run the model
        setwd(dir)
        tryCatch({
          shell(.LW_EXE_FILE_NAME)
        }, error = function(err){
          cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
               "Check that '",exe,"' exists and was compiled properly.")
          setwd(currDir)
          return(NULL)
        }, error = function(err){
          cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
               "Check that '",exe,"' exists and was compiled properly.")
          setwd(currDir)
          return(NULL)
        })
        setwd(currDir)
        bio$lw[[sex]] <<- readModelOutput(dir, .LW_EXE_BASE_NAME)
      }
    }else{
      if(!createLengthWeightDatafile(areas, 3, surveys, multLen, multWt)){
        return(NULL)
      }
      # Change to the directory to run the model
      setwd(dir)
      tryCatch({
        shell(.LW_EXE_FILE_NAME)
      }, error = function(err){
        cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
             "Check that '",exe,"' exists and was compiled properly.")
        setwd(currDir)
        return(NULL)
      }, error = function(err){
        cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
             "Check that '",exe,"' exists and was compiled properly.")
        setwd(currDir)
        return(NULL)
      })
      setwd(currDir)
      bio$lw[[1]] <<- readModelOutput(dir, .LW_EXE_BASE_NAME)
      bio$lw[[2]] <<- NULL # In case there was a previous split-sex run
    }
  }
  if(model == 2){
    # Check that executable exists
    exe <- file.path(dir, .VONB_EXE_FILE_NAME)
    if(!file.exists(exe)){
      cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - '",exe,"' does not exist. Compile it and try again.")
      return(NULL)
    }
    if(splitSex){
      for(sex in 1:2){
        if(!createVonbDatafile(areas, sex, ages, surveys, multLen)){
          return(NULL)
        }
        # Change to the directory to run the model
        setwd(dir)
        tryCatch({
          shell(.VONB_EXE_FILE_NAME)
        }, error = function(err){
          cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
               "Check that '",exe,"' exists and was compiled properly.")
          setwd(currDir)
          return(NULL)
        }, error = function(err){
          cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
               "Check that '",exe,"' exists and was compiled properly.")
          setwd(currDir)
          return(NULL)
        })
        setwd(currDir)
        bio$vonb[[sex]] <<- readModelOutput(dir, .VONB_EXE_BASE_NAME)
      }
    }else{
      if(!createVonbDatafile(areas, 3, ages, surveys, multLen)){
        return(NULL)
      }
      # Change to the directory to run the model
      setwd(dir)
      tryCatch({
        shell(.VONB_EXE_FILE_NAME)
      }, error = function(err){
        cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
             "Check that '",exe,"' exists and was compiled properly.")
        setwd(currDir)
        return(NULL)
      }, error = function(err){
        cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
             "Check that '",exe,"' exists and was compiled properly.")
        setwd(currDir)
        return(NULL)
      })
      setwd(currDir)
      bio$vonb[[1]] <<- readModelOutput(dir, .VONB_EXE_BASE_NAME)
      bio$vonb[[2]] <<- NULL # In case there was a previous split-sex run
    }
  }
  if(model == 3){
    # Check that executable exists
    exe <- file.path(dir, .MA_EXE_FILE_NAME)
    if(!file.exists(exe)){
      cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - '",exe,"' does not exist. Compile it and try again.")
      return(NULL)
    }
    if(splitSex){
      for(sex in 1:2){
        if(!createMaturityAgeDatafile(areas, sex, surveys, multLen)){
          return(NULL)
        }
        # Change to the directory to run the model
        setwd(dir)
        tryCatch({
          shell(.MA_EXE_FILE_NAME)
        }, error = function(err){
          cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
               "Check that '",exe,"' exists and was compiled properly.")
          setwd(currDir)
          return(NULL)
        }, error = function(err){
          cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
               "Check that '",exe,"' exists and was compiled properly.")
          setwd(currDir)
          return(NULL)
        })
        setwd(currDir)
        bio$ma[[sex]] <<- readModelOutput(dir, .MA_EXE_BASE_NAME)
      }
    }else{
      if(!createMaturityAgeDatafile(areas, 3, surveys, multLen)){
        return(NULL)
      }
      # Change to the directory to run the model
      setwd(dir)
      tryCatch({
        shell(.MA_EXE_FILE_NAME)
      }, error = function(err){
        cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
             "Check that '",exe,"' exists and was compiled properly.")
        setwd(currDir)
        return(NULL)
      }, error = function(err){
        cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error running model.",
             "Check that '",exe,"' exists and was compiled properly.")
        setwd(currDir)
        return(NULL)
      })
      setwd(currDir)
      bio$ma[[1]] <<- readModelOutput(dir, .MA_EXE_BASE_NAME)
      bio$ma[[2]] <<- NULL # In case there was a previous split-sex run
    }
  }
}

readModelOutput <- function(dir, name){
  # Read in the input DAT file and the output REP file
  # for a model run in the directory 'dir' with the model 'name'.
  # Returns a list of length two, the first element is a two-column matrix
  #  containing the lengths in the first column and the weights in the second.
  # The second list element is a vector of length two, with the parameter estimates.

  fileDat <- file.path(dir, paste0(name,".dat"))
  fileRep <- file.path(dir, paste0(name,".rep"))
  out <- NULL

  tmp  <- read.table(file = fileDat, sep="\n")
  nobs <- tmp[1,]
  tmp  <- matrix(tmp[-c(1,nrow(tmp)),], ncol=2, nrow=nobs)
  out[[1]] <- tmp
  out[[2]] <- read.table(file = fileRep, sep="\n")
  return(out)
}

getLW <- function(sex, areas, surveys){
  # Extract all non-null length/weight data
  # return a data frame with all lengthed and weighted fish for the years and areas given
  # sex is the sex to extract data for, 1=male, 2=female, anything else=combined
  #  out <- d[d$year %in% years,]

  d <- biodata
  out <- d[d$PMFC %in% areas,]
  if(nrow(out) == 0){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - There areas you chose so not exist in the data.\n")
    return(NULL)
  }
  out <- out[out$SSID %in% surveys,]
  if(nrow(out) == 0){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - There surveys you chose so not exist in the data.\n")
    return(NULL)
  }
  if(sex == 1 || sex == 2){
    out <- out[out$sex == sex,]
  } # else don't bother with sex discrimination
  out <- out[!is.na(out$len),]
  out <- out[!is.na(out$wt),]
  return(out)
}

getMA <- function(sex, areas, surveys, matlevel = 3){
  # Extract all non-null maturity and age data
  # return a data frame with all fish with length and proportion mature
  #  for the years and areas given
  # sex is the sex to extract data for, 1=male, 2=female, anything else=combined
  # matlevel is the maturity level to consider mature,
  #  this value and higher will be considered mature.
  #  out <- d[d$year %in% years,]
  d <- biodata
  out <- d[d$PMFC %in% areas,]
  if(nrow(out) == 0){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - There areas you chose so not exist in the data.\n")
    return(NULL)
  }
  out <- out[out$SSID %in% surveys,]
  if(nrow(out) == 0){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - There surveys you chose so not exist in the data.\n")
    return(NULL)
  }
  if(sex == 1 || sex == 2){
    out <- out[out$sex == sex,]
  } # else don't bother with sex discrimination
  out <- out[!is.na(out$age),]
  out <- out[!is.na(out$mat),]
  # Now calculate the proportion at each length which are mature
  dtf <- out$mat >= matlevel
  df <- table(out$age, dtf)
  df <- addmargins(df, 2)          # add row sums for TRUE/FALSE counts
  df <- cbind(df, df[,2] / df[,3]) # add proportions which are TRUE (mature)

  # Bind together the lengths and their proportion mature
  out <- cbind(as.numeric(rownames(df)), df[,4])
  colnames(out) <- c("age","mat")
  return(as.data.frame(out))
}

getLA <- function(sex, areas, ages, surveys){
  # Extract all non-null length/age data
  # return a data frame with all lengthed and aged fish
  # sex is the sex to extract data for, 1=male, 2=female, anything else=combined
  #  out <- d[d$year %in% years,]
  d <- biodata
  out <- d[d$PMFC %in% areas,]
  if(nrow(out) == 0){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - There areas you chose so not exist in the data.\n")
    return(NULL)
  }
  out <- out[out$SSID %in% surveys,]
  if(nrow(out) == 0){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - There surveys you chose so not exist in the data.\n")
    return(NULL)
  }
  if(sex == 1 || sex == 2){
    out <- out[out$sex == sex,]
  } # else don't bother with sex discrimination
  out <- out[!is.na(out$len),]
  out <- out[!is.na(out$age),]
  out <- out[out$age %in% ages,]
  return(out)
}

createVonbDatafile <- function(areas, sex, ages, survey,
                               multLen = 1){
  # Extract all non-null length/age data and write to the data file.
  # areas is a vector of areas i.e. 3C, 3D, etc
  # sex=1 is male, sex=2 is female, anything else=combined
  # ages is a vector of ages to include in ther extraction
  # multLen is a multiplier for the length for unit conversion.
  # survey is a key number from the global surveyKeys declared at the bottom of this file.
  la <- getLA(sex, areas, ages, survey)
  if(is.null(la)){
    return(FALSE)
  }
  nobs <- nrow(la)
  lengths <- la$len * as.numeric(multLen)
  ages <- la$age
  fn <- file.path(.BIODATA_DIR_NAME, .VONB_DAT_FILE_NAME)
  if(sex == 1){
    sexStr <- "males"
  }else if(sex == 2){
    sexStr <- "females"
  }else{
    sexStr <- "combined sexes"
  }
  # Write the datafile
  write(paste0("# Length-age data file for ",sexStr), fn, ncolumns = 1)
  write("# Number of observations", fn, ncolumns = 1, append = TRUE)
  write(nobs, fn, ncolumns = 1, append = TRUE)
  write("\n# Observed lengths (mm)", fn, ncolumns = 1, append = TRUE)
  write(lengths, fn, ncolumns = 1, append=TRUE)
  write("\n# Observed ages\n", fn, ncolumns = 1, append = TRUE)
  write(ages, fn, ncolumns = 1, append=TRUE)
  write("\n999\n", fn, ncolumns = 1, append = TRUE)
  cat0("Wrote the file ",fn," to disk.\n")
  return(TRUE)
}

createLengthWeightDatafile <- function(areas, sex, surveys,
                                       multLen = 1, multWt = 1){
  # Extract all non-null length/weight data and write to the data file.
  # areas is a vector of areas i.e. 3C, 3D, etc
  # sex=1 is male, sex=2 is femal, anything else=combined
  # multLen and multWt are multipliers for the length and weight for unit conversion.
  # survey is a key number from the global surveyKeys declared at the bottom of this file.
  lw <- getLW(sex, areas, surveys)
  if(is.null(lw)){
    return(FALSE)
  }
  nobs <- nrow(lw)
  lengths <- lw$len * as.numeric(multLen)
  weights <- lw$wt * as.numeric(multWt)
  #data <- rbind(nobs, lengths, weights, 999)
  fn <- file.path(.BIODATA_DIR_NAME, .LW_DAT_FILE_NAME)
  if(sex == 1){
    sexStr <- "males"
  }else if(sex == 2){
    sexStr <- "females"
  }else{
    sexStr <- "combined sexes"
  }
  # Write the datafile
  write(paste0("# Length-weight data file for ",sexStr), fn, ncolumns = 1)
  write("# Number of observations", fn, ncolumns = 1, append = TRUE)
  write(nobs, fn, ncolumns = 1, append = TRUE)
  write("\n# Observed lengths (mm)", fn, ncolumns = 1, append = TRUE)
  write(lengths, fn, ncolumns = 1, append=TRUE)
  write("\n# Observed weights (g)\n", fn, ncolumns = 1, append = TRUE)
  write(weights, fn, ncolumns = 1, append=TRUE)
  write("\n999\n", fn, ncolumns = 1, append = TRUE)
  cat0("Wrote the file ",fn," to disk.\n")
  return(TRUE)
}

createMaturityAgeDatafile <- function(areas, sex, surveys, multLen = 1){
  # Extract all non-null maturity and age data and write to the data file.
  # areas is a vector of areas i.e. 3C, 3D, etc
  # sex=1 is male, sex=2 is female, anything else=combined
  # multLen is a multiplier for the length for unit conversion.
  # survey is a key number from the global surveyKeys declared at the bottom of this file.

  #lm <- getLM(sex, areas, survey)
  ma <- getMA(sex, areas, surveys)
  if(is.null(ma)){
    return(FALSE)
  }
  nobs <- nrow(ma)
  #lengths <- lm$len * as.numeric(multLen)
  ages <- ma$age
  maturities <- ma$mat
  #data <- rbind(nobs, lengths, weights, 999)
  fn <- file.path(.BIODATA_DIR_NAME, .MA_DAT_FILE_NAME)
  if(sex == 1){
    sexStr <- "males"
  }else if(sex == 2){
    sexStr <- "females"
  }else{
    sexStr <- "combined sexes"
  }
  # Write the datafile
  write(paste0("# Maturity-at-age data file for ",sexStr), fn, ncolumns = 1)
  write("# Number of observations", fn, ncolumns = 1, append = TRUE)
  write(nobs, fn, ncolumns = 1, append = TRUE)
  write("\n# Observed ages", fn, ncolumns = 1, append = TRUE)
  write(ages, fn, ncolumns = 1, append=TRUE)
  write("\n# Observed maturities\n", fn, ncolumns = 1, append = TRUE)
  write(maturities, fn, ncolumns = 1, append=TRUE)
  write("\n999\n", fn, ncolumns = 1, append = TRUE)
  cat0("Wrote the file ",fn," to disk.\n")
  return(TRUE)
}

.loadBiodata <- function(){
  # Load the biological data in the file pointed to by the global 'biodataFile'
  if(!exists("biodataFile") || biodataFile == ""){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"The file has not been chosen yet. Use the GUI to choose a file.")
    return(NULL)
  }
  load(biodataFile) #, envir = sys.frame(sys.parent(0)))
  # The following assumes that the newly loaded data is the first
  # object, or only object in this (function's) environment.
  biodata <<- eval(parse(text=ls()[1]))
  cat0(.PROJECT_NAME,"->",getCurrFunc(),"Biological data loaded from the file '",biodataFile,"'")
  cat0(.PROJECT_NAME,"->",getCurrFunc()," and stored in global object 'biodata'.")
  return(NULL)
}

getAgedData <- function(d, years, areas, surveys=0){
  # return a data frame with all aged fish for the dataset for the years and PMFC areas given
  # years is assumed to be a vector
  # If surv == 0, commercial ages will be returned, if it is anything else, it is the ssid (survey series id)
  out <- d[d$year %in% years,]
  out <- out[out$PMFC %in% areas,]
  out <- out[out$SSID %in% surveys,]
  out <- out[!is.na(out$age),]
  return(out)
}

getAgeData <- function(dat, minAge=1, maxAge=25, gear=NULL){
  # return a matrix in iscam format with unique years as rows and age proportions
  # 'dat' is a list of data frames, one for each area you wish to set as a unique area
  # the order of the data frames in 'dat' will dictate the value used for the area
  # variable in the output, i.e. the first will be given 1, the second 2, ...

  if(is.null(gear)){
    cat0(getCurrFunc(),"You must supply a gear number. Generally commercial=1 and surveys are 2,3....")
    return(NULL)
  }
  out <- data.frame()
  for(area in 1:length(dat)){
    d <- dat[[area]]
    uniqueYears <- sort(unique(d$year))
    for(year in 1:length(uniqueYears)){
      yr <- uniqueYears[year]
      records <- d[d$year==yr,]
      for(sex in 1:2){
        sexRecords <- records[records$sex==sex,]
        nData <- nrow(sexRecords) # the number of records for this year and sex
        propTable <- table(sexRecords$age)
        agesPresent <- as.numeric(names(propTable))
        ## year gear area group sex | age data columns (numbers or proportions)
        tmp <- c(uniqueYears[year],gear,area,1,sex,nData) # This code sets group to 1 always
        for(age in minAge:maxAge){
          if(!(age %in% agesPresent)){
            tmp <- c(tmp,0)
          }else{
            tmp <- c(tmp,as.numeric(propTable[agesPresent==age]))
          }
        }
        out <- rbind(out, tmp)
      }
    }
  }
  return(out)
}

getIscamAgeData <- function(){
  # This copies the age data into the clipboard for easy pasting into iscam data file.
  # This is split sex only at the moment.
  wcviSurvAges <- getAgedData(d, years, areas, surveys=4) # surv=4 is WCVI Synoptic
  wcviSurvAgeData <- getAgeData(list(wcviSurvAges), minAge=1, maxAge=25, gear=2)

  # No ages for shrimp trawl survey, this will break of you try it with no age data
  #wcviShrimpSurvAges <- getAgedData(d, years, areas, surveys=7) # surv=7 is WCVI Shrimp trawl
  #wcviShrimpSurvAgeData <- getAgeData(list(wcviShrimpSurvAges), minAge=1, maxAge=25, gear=3)

  commAges <- getAgedData(d, years, areas, surveys=0)
  commAgeData <- getAgeData(list(commAges), minAge=1, maxAge=25, gear=1)

  names(commAgeData) <- names(wcviSurvAgeData)
  #allAgeData <- rbind(commAgeData, wcviSurvAgeData, wcviShrimpSurvAgeData)
  allAgeData <- rbind(commAgeData, wcviSurvAgeData)
  write.table(allAgeData, "clipboard", sep=" ", row.names=FALSE, col.names=FALSE)
  cat0("The clipboard contains the data in iscam format.\n")

  # HECATE STRAIT SURVEY AND COMMERCIAL AGES
  # Need to add this once the aging is complete
  #years <- c(2005,2007,2009,2011,2013)
  #hecaSurvAges <- getAgedData(d, years, areas, surveys=3)
  #hecaSurvAges <- getSurveyAges(d, 3)
}

# Load the data if it hasn't been yet.
#if(!exists("ca") || !exists("d")){
#  cat0("Loading raw data..")
#  loadRawData()
#}else{
#  cat0("Not loading raw data, using existing objects 'd' (biological) and 'ca' (catch)")
#}

#lwvDir <- "length-weight-vonb"

#years <- sort(unique(d$year))
#areas <- c("3C", "3D")
#areas <- c("5C", "5D") # For the hecate strait data

#getIscamAgeData()
# You need to run this once for each sex if splitting sex, as ADMB
# program is simple and requires a single file name input.
#getLengthWeightData(sex = 1, convertLen = TRUE, convertWt = TRUE)
#getLengthWeightData(sex = 2, convertLen = TRUE, convertWt = TRUE)

.parseAges <- function(ageStr){
  # parse the ageStr string which is either a comma-seperated list of
  # ages, e.g. 1,2,3,4,5,10 or a range of ages, eg. 1:25
  # Return a vector of ages listed in the ageStr string
  # If ageStr has an error in it, an error message will be shown and
  # NULL will be returned.
  # If ageStr is a null string (""), NULL will be returned
  currFuncName <- getCurrFunc()

  ageStr <- paste0("c(",ageStr,")")
  ages <- NULL
  tryCatch({
    ages <- eval(parse(text = ageStr))
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Warning - problem with your age list.")
    cat0(.PROJECT_NAME,"->",currFuncName,war$message)
    return(NULL)
  }, error = function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"Error - problem with your age list.")
    cat0(.PROJECT_NAME,"->",currFuncName,err$message)
    return(NULL)
  })
  return(ages)
}

.parseAreas <- function(areaStr){
  # parse the areaStr string which is a comma-seperated list of
  # text areas, e.g. 3C,3D,5C
  # Return a vector of areas listed in the areaStr string
  # If areaStr has an error in it, an error message will be shown and
  # NULL will be returned.
  # If areaStr is a null string (""), NULL will be returned
  currFuncName <- getCurrFunc()

  tryCatch({
    areas <- strsplit(areaStr,",")[[1]]
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Warning - problem with your area list.")
    cat0(.PROJECT_NAME,"->",currFuncName,war$message)
    return(NULL)
  }, error = function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"Error - problem with your area list.")
    cat0(.PROJECT_NAME,"->",currFuncName,err$message)
    return(NULL)
  })
  # Remove empty strings, in case there were multiple commas in a row
  areas <- areas[areas!=""]
  # Capitalize all letters, as GFBio stores them this way
  areas <- toupper(areas)
  if(length(areas) == 0){
    return(NULL)
  }
  return(areas)
}

.parseSurveys <- function(surveyStr){
  # parse the surveyStr string which is a comma-seperated list of
  # survey key codes (surveyKeys object)
  # Return a vector of surveys listed in the surveyStr string
  # If surveyStr has an error in it, an error message will be shown and
  # NULL will be returned.
  # If surveyStr is a null string (""), NULL will be returned
  # If one or more of the values in surveyStr is not in surveyKeys,
  #  an error message will be printed out and NULL will be returned
  currFuncName <- getCurrFunc()

  tryCatch({
    surveys <- strsplit(surveyStr,",")[[1]]
  }, warning = function(war){
    cat0(.PROJECT_NAME,"->",currFuncName,"Warning - problem with your surveys list.")
    cat0(.PROJECT_NAME,"->",currFuncName,war$message)
    return(NULL)
  }, error = function(err){
    cat0(.PROJECT_NAME,"->",currFuncName,"Error - problem with your surveys list.")
    cat0(.PROJECT_NAME,"->",currFuncName,err$message)
    return(NULL)
  })
  # Remove empty strings, in case there were multiple commas in a row
  surveys <- surveys[surveys!=""]
  if(length(surveys) == 0){
    return(NULL)
  }
  if(!all(surveys %in% surveyKeys)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Warning - not all of your survey codes are in surveyKeys object.")
    return(NULL)
  }
  return(as.numeric(surveys))
}

surveyKeys <<- c(0,1,2,3,4,5,6,7,8,9,10,
                 11,12,13,14,15,16,17,18,19,20,
                 21,22,23,24,25,26,27,28,29,30,
                 31,32,33,34,35,36,37,38,39,40,
                 41,42,43,44,45,46,47,67,68,69,70,
                 71,72,73,74,75)
surveyValues <<- c("Individual survey without a series",
                   "Queen Charlotte Sound Synoptic Survey",
                   "Hecate Strait Multispecies Assemblage Survey",
                   "Hecate Strait Synoptic Survey",
                   "West Coast Vancouver Island Synoptic Survey",
                   "Hecate Strait Pacific Cod Monitoring Survey",
                   "Queen Charlotte Sound Shrimp Survey",
                   "West Coast Vancouver Island Shrimp Survey",
                   "1997 West Coast QCI Rockfish Survey (single survey series)",
                   "1996 West Coast VI Rockfish Survey (single survey series)",
                   "1995 QC Sound Rockfish Survey",
                   "West Coast Vancouver Island Thornyhead Survey",
                   "Combined WCVI/Hecate/QC Sound Synoptic",
                   "Inshore Rockfish Longline Survey",
                   "IPHC Longline Survey",
                   "Lingcod YOY Trawl Survey",
                   "West Coast Haida Gwaii Synoptic Survey",
                   "IPHC Longline Survey: Vancouver Station (3CD)",
                   "IPHC Longline Survey: Goose and James Stations (5AB)",
                   "IPHC Longline Survey: Charlotte Station (5CD)",
                   "WCVI Synoptic Cdn Vancouver Region",
                   "Goose Island Gully Retrospective Study",
                   "PHMA Rockfish Longline Survey - Outside North",
                   "WCVI Thornyhead (no area strata)",
                   "WCVI Thornyhead (2 depth/no area strata)",
                   "WCVI Thornyhead (2 depth/2 area strata)",
                   "G.B. Reed Historic Goose Island Gully Surveys",
                   "WCVI Synoptic in Triennial Region",
                   "QCS Historic GB Reed + Modern Viking Storm Time Series",
                   "QCS Synoptic Surveys Restratified for 2008 POP Analysis (Paul Starr et al.)",
                   "WCHG Synoptic Without Deepest Stratum (800 - 1300m)",
                   "WCVI Synoptic with modified south/north boundaries for Jennifer Bolt",
                   "Restratification of Goose Island Gully for the 2009 POP Assessment.",
                   "Restratification of 1995 Ocean Selector and Frosti GIG surveys using southern QCS synoptic strata.",
                   "Strait of Georgia ERI (Hake/Pollock) Survey",
                   "Sablefish Research and Assessment Survey",
                   "PHMA Rockfish Longline Survey - Outside South",
                   "WCHG 1997, 2006+ 180 to 500m",
                   "WCVI 1996, 2004+ 125 to 500m",
                   "IRF Longline Survey (North)",
                   "IRF Longline Survey (South)",
                   "Sablefish Inlet Standardized",
                   "Sablefish Offshore Standardized",
                   "Sablefish Stratified Random",
                   "QCS Synoptic Surveys footprint reduced to match industry proposal (30/11/2011)",
                   "Strait of Georgia Synoptic Survey",
                   "Neocaligus Shrimp Trawl Surveys",
                   "Hake Stock Delineation Survey",
                   "WCVI and QCS Shrimp Survey",
                   "Joint Can/US Hake Acoustic Survey",
                   "QCS Synoptic For Big Skate Assessment",
                   "WCVI Synoptic For Big Skate Assessment",
                   "Hecate Strait Synoptic For Big Skate Assessment",
                   "QCS Synoptic For Longnose Skate Assessment",
                   "WCVI Synoptic For Longnose Skate Assessment",
                   "Hecate Strait Synoptic For Longnose Skate Assessment",
                   "WCHG Synoptic For Longnose Skate Assessment")
surveyList <<- data.frame(key = surveyKeys, value = surveyValues)
colnames(surveyList) <- c("SURVEY_SERIES_ID", "SURVEY_SERIES_DESC")
