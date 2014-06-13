source("utilities.r")

loadRawData <- function(){
  # Load the raw biological and catch data from files produced by Rowan.
  # See rowan_extract_notes.txt for more information on the data structure and queries.
  load("bio602_march_extraction.rdata", envir=.GlobalEnv)
  load("cat602.rdata", envir=.GlobalEnv)
  d  <<- bio602
  ca <<- cat602
}

getAgedData <- function(d, years, areas, surv=0){
  # return a data frame with all aged fish for the dataset for the years and PMFC areas given
  # years is assumed to be a vector
  # If surv == 0, commercial ages will be returned, if it is anything else, it is the ssid (survey series id)
  out <- d[d$year %in% years,]
  out <- out[out$PMFC %in% areas,]
  out <- out[out$SSID==surv,]
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
  wcviSurvAges <- getAgedData(d, years, areas, surv=4) # surv=4 is WCVI Synoptic
  wcviSurvAgeData <- getAgeData(list(wcviSurvAges), minAge=1, maxAge=25, gear=2)

  # No ages for shrimp trawl survey, this will break of you try it with no age data
  #wcviShrimpSurvAges <- getAgedData(d, years, areas, surv=7) # surv=7 is WCVI Shrimp trawl
  #wcviShrimpSurvAgeData <- getAgeData(list(wcviShrimpSurvAges), minAge=1, maxAge=25, gear=3)

  commAges <- getAgedData(d, years, areas, surv=0)
  commAgeData <- getAgeData(list(commAges), minAge=1, maxAge=25, gear=1)

  names(commAgeData) <- names(wcviSurvAgeData)
  #allAgeData <- rbind(commAgeData, wcviSurvAgeData, wcviShrimpSurvAgeData)
  allAgeData <- rbind(commAgeData, wcviSurvAgeData)
  write.table(allAgeData, "clipboard", sep=" ", row.names=FALSE, col.names=FALSE)
  cat0("The clipboard contains the data in iscam format.\n")

  # HECATE STRAIT SURVEY AND COMMERCIAL AGES
  # Need to add this once the aging is complete
  #years <- c(2005,2007,2009,2011,2013)
  #hecaSurvAges <- getAgedData(d, years, areas, surv=3)
  #hecaSurvAges <- getSurveyAges(d, 3)
}

getLW <- function(d, sex, years, areas, surv=0){
  # return a data frame with all lengthed and weighted fish for the years and PMFC areas given
  # sex is the sex to extract data for, 1=male, 2=female, anything else=combined
  # years is assumed to be a vector
  # If surv == 0, commercial ages will be returned, if it is anything else, it is the ssid (survey series id)
  out <- d[d$year %in% years,]
  out <- out[out$PMFC %in% areas,]
  out <- out[out$SSID==surv,]
  if(sex == 1 || sex == 2){
    out <- out[out$sex==sex,]
  }
  out <- out[!is.na(out$len),]
  out <- out[!is.na(out$wt),]
  return(out)
}

getLengthWeightData <- function(sex = 1, convertLen = TRUE, convertWt = TRUE){
  # Get all length/weight data.
  # sex is the sex to extract data for, 1=male, 2=female, anything else=combined
  # If convertLen is TRUE, then the lengths will be multiplied by 10 (cm->mm)
  # If convertWt is TRUE, then the weights will be multiplied by 1000 (kg->g)
  lw <- getLW(d, sex, years, areas, surv=4) # surv=4 is WCVI synoptic
  nobs <- nrow(lw)
  lengths <- lw$len
  if(convertLen){
    lengths <- lengths * 10
  }
  weights <- lw$wt
  if(convertWt){
    weights <- weights * 1000
  }
  #data <- rbind(nobs, lengths, weights, 999)
  fn <- file.path(lwvDir, "lengthweight.dat")
  write("# Number of observations", fn, ncolumns = 1)
  write(nobs, fn, ncolumns = 1, append = TRUE)
  write("\n# Observed lengths (mm)", fn, ncolumns = 1, append = TRUE)
  write(lengths, fn, ncolumns = 1, append=TRUE)
  write("\n# Observed weights (g)\n", fn, ncolumns = 1, append = TRUE)
  write(weights, fn, ncolumns = 1, append=TRUE)
  write("\n999\n", fn, ncolumns = 1, append = TRUE)
  cat0("Wrote the file ",fn," to disk.\n")
}

# Load the data if it hasn't been yet.
if(!exists("ca") || !exists("d")){
  cat0("Loading raw data..")
  loadRawData()
}else{
  cat0("Not loading raw data, using existing objects 'd' (biological) and 'ca' (catch)")
}

lwvDir <- "length-weight-vonb"

years <- sort(unique(d$year))
areas <- c("3C", "3D")
#areas <- c("5C", "5D") # For the hecate strait data

#getIscamAgeData()
# You need to run this once for each sex if splitting sex, as ADMB
# program is simple and requires a single file name input.
getLengthWeightData(sex = 1, convertLen = TRUE, convertWt = TRUE)
#getLengthWeightData(sex = 2, convertLen = TRUE, convertWt = TRUE)


# The surv argument above (in getIscamAgeData) is based on this lookup table:
## SURVEY_SERIES_ID	SURVEY_SERIES_DESC
## 0	Individual survey without a series
## 1	Queen Charlotte Sound Synoptic Survey
## 2	Hecate Strait Multispecies Assemblage Survey
## 3	Hecate Strait Synoptic Survey
## 4	West Coast Vancouver Island Synoptic Survey
## 5	Hecate Strait Pacific Cod Monitoring Survey
## 6	Queen Charlotte Sound Shrimp Survey
## 7	West Coast Vancouver Island Shrimp Survey
## 8	1997 West Coast QCI Rockfish Survey (single survey series)
## 9	1996 West Coast VI Rockfish Survey (single survey series)
## 10	1995 QC Sound Rockfish Survey
## 11	West Coast Vancouver Island Thornyhead Survey
## 12	Combined WCVI/Hecate/QC Sound Synoptic
## 13	Inshore Rockfish Longline Survey
## 14	IPHC Longline Survey
## 15	Lingcod YOY Trawl Survey
## 16	West Coast Haida Gwaii Synoptic Survey
## 17	IPHC Longline Survey: Vancouver Station (3CD)
## 18	IPHC Longline Survey: Goose and James Stations (5AB)
## 19	IPHC Longline Survey: Charlotte Station (5CD)
## 20	WCVI Synoptic Cdn Vancouver Region
## 21	Goose Island Gully Retrospective Study
## 22	PHMA Rockfish Longline Survey - Outside North
## 23	WCVI Thornyhead (no area strata)
## 24	WCVI Thornyhead (2 depth/no area strata)
## 25	WCVI Thornyhead (2 depth/2 area strata)
## 26	G.B. Reed Historic Goose Island Gully Surveys
## 27	WCVI Synoptic in Triennial Region
## 28	QCS Historic GB Reed + Modern Viking Storm Time Series
## 29	QCS Synoptic Surveys Restratified for 2008 POP Analysis (Paul Starr et al.)
## 30	WCHG Synoptic Without Deepest Stratum (800 - 1300m)
## 31	WCVI Synoptic with modified south/north boundaries for Jennifer Bolt
## 32	Restratification of Goose Island Gully for the 2009 POP Assessment.
## 33	Restratification of 1995 Ocean Selector and Frosti GIG surveys using southern QCS synoptic strata.
## 34	Strait of Georgia ERI (Hake/Pollock) Survey
## 35	Sablefish Research and Assessment Survey
## 36	PHMA Rockfish Longline Survey - Outside South
## 37	WCHG 1997, 2006+ 180 to 500m
## 38	WCVI 1996, 2004+ 125 to 500m
## 39	IRF Longline Survey (North)
## 40	IRF Longline Survey (South)
## 41	Sablefish Inlet Standardized
## 42	Sablefish Offshore Standardized
## 43	Sablefish Stratified Random
## 44	QCS Synoptic Surveys footprint reduced to match industry proposal (30/11/2011)
## 45	Strait of Georgia Synoptic Survey
## 46	Neocaligus Shrimp Trawl Surveys
## 47	Hake Stock Delineation Survey
## 67	WCVI and QCS Shrimp Survey
## 68	Joint Can/US Hake Acoustic Survey
## 69	QCS Synoptic For Big Skate Assessment
## 70	WCVI Synoptic For Big Skate Assessment
## 71	Hecate Strait Synoptic For Big Skate Assessment
## 72	QCS Synoptic For Longnose Skate Assessment
## 73	WCVI Synoptic For Longnose Skate Assessment
## 74	Hecate Strait Synoptic For Longnose Skate Assessment
## 75	WCHG Synoptic For Longnose Skate Assessment
