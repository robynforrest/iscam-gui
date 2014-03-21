#**********************************************************************************
# ccamTables.r
# This file contains the code for writing main body tables to disk.
# This file assumes that an object called 'opList' exists and is a valid opList as
#  described in loadScenarios.r
#
# Author            : Chris Grandin
# Development Date  : January 2012
# RF: Modified Nov 2013
# CG: Added table.mcmc.mpd November 2013
#
#**********************************************************************************

table.mcmc.mpd <- function(mcmcData,
                           burnin          = 10,
                           probs           = c(0.025,0.5,0.975),
                           mpdData         = NULL,
                           tableName       = NULL,
                           colLabels       = NULL,
                           formatOut       = "%1.4f",  # 4 decimal places shown regarless of what roundDec is
                           roundDec        = 4,        # Round to 4 decimal places
                           formatThousands = FALSE){
  # mcmcData is a years-column, mcmc-sample from posterior row matrix with the burnin not yet removed.
  # If the number of rows in mcmcData exceeds the number of years, a warning will be given and the
  #   extra rows (which are projection years) will be removed.
  # burnIn is the number of rows to remove from the beginning of the mcmcData matrix. If greater than number of
  #  rows of mcmcData, an error will be given.
  # mpdData is a vector, or a matrix (in which case only the first row will be used by this function.
  # probs are the probablities used in the quantile function through getMCMCQuantiles
  # tableName is the name of the table to be stored. If null, an error will be given. .csv will be appended to this.
  # years will be used as the names for the columns in the table. If NULL, an error will be given.
  # If th length of the years vector does not match with the number of columns of mcmcData or
  # number of elements in mpdDat, an error will be given.
  # roundDec is the number of decimal points to round to
  # formatThousands puts a comma seperator in thousands values if TRUE.

  if(is.null(tableName)){
    cat("table.mcmc.mpd: Error - You didn't supply a filename.\n")
    return()
  }
  if(is.null(colLabels)){
    cat("table.mcmc.mpd: Error - For ",tableName,", you didn't supply the column labels.\n",sep="")
    return()
  }
  if(burnin < 0 || burnin >= nrow(mcmcData)){
    cat("table.mcmc.mpd: Error - For ",tableName,", the burnin value is not in within the number of rows in mcmcData.\n",sep="")
    return()
  }
  if(ncol(mcmcData) < length(colLabels)){
    cat("table.mcmc.mpd: Error - For ",tableName,", the length of vector 'colLabels' is greater than the number of columns in mcmcData.\n",sep="")
    return()
  }
  if(ncol(mcmcData) > length(colLabels)){
    cat("table.mcmc.mpd: Warning - For ",tableName,", the length of vector 'colLabels' is less than the number of columns in mcmcData.\n",
        "                The extra rows are assumed to be projections unwanted in the table so they have been removed.\n",sep="")
    mcmcData <- mcmcData[,-((length(colLabels)+1):length(mcmcData))]
  }
  # Take first row of mpdData if it is a matrix
  if(is.matrix(mpdData)){
    mpdData <- mpdData[1,]
  }
  if(length(mpdData) != length(colLabels)){
    cat("table.mcmc.mpd: Error - For ",tableName,", the length of vector 'colLabels' doesn't match the length of vector 'mpdData'.\n",sep="")
    return()
  }

  tableName <- paste0(tableName,".csv")
  fileName  <- paste0(tabDir,tableName)

  # Apply burnin, remove first 'burnin' rows from mcmcData
  mcmcDataBurnedIn <- mcmcData[-(1:burnin),]
  # Get the mcmc quantiles
  quants <- sapply(as.data.frame(mcmcDataBurnedIn), quantile, probs = probs)

  # Add on the MPD output
  tableData <- rbind(quants, mpdData)
  # Make it look like a table
  tableData <- t(tableData)

  # Format the values
  tableOut <- NULL
  for(col in 1:ncol(tableData)){
    tableOut <- cbind(tableOut, rsprintf(tableData[,col], roundDec = roundDec, formatOut = formatOut, formatThousands = formatThousands))
  }

  colnames(tableOut) <- colnames(tableData)
  rownames(tableOut) <- colLabels
  write.csv(tableOut, fileName)
}

#This table calculates medians and quantiles for the control points (forecast and hindcast variables that are not affected by future (2014)TAC)
#	and performance measures (forecast variables that are affected by future (2014) TAC)
table.projections <- function(){
	dd <- A$mcproj #read.table(file=fn, head=T)
	tac <- unique(dd$tac)
	quan <- c(0.025, .25, .5, 0.75, 0.975)
	med <- 0.5
	
	#Values that do not change with TAC
	ctlpts<-c(2,5,8,12,14,16,18,20,22)
	CtlPts_quan <- NULL
	
	#Values that change with TAC
	Perf_med <- NULL
	B2015_quan  <- NULL
	F2014_quan  <- NULL
	
	#Values that do not change with TAC
	ctlnames=paste(colnames(dd[ctlpts]))
	d <- subset(dd, tac==0)	#Take only values from one TAC - these control points are insensitive to TAC
	nsamp <- length(d[,1])
        d<-d[(Burn+1):nsamp,]
	jj=0
	for(i in ctlpts) {
		jj=jj+1
		CtlPts_quan=rbind(CtlPts_quan,c(ctlnames[jj],quantile(dd[,i], na.rm=T,quan)))
  	}
	
	#Values that change with TAC
	for(i in tac){
		d <- subset(dd, tac==i)
		d<-d[(Burn+1):nsamp,]	  #Remove burn-in samples once subset has been done
		#print(nsamp)
		#print(length(d[,1]))
		
		#Medians
		B2015 <- quantile(d$B2015, na.rm=T,med)	  #Biomass in 2015
		F2014  <- quantile(d$F2014, na.rm=T,med)	 #F in 2014
		B2015B2014   <- quantile(d$B2015B2014, na.rm=T,med)	#B2015/B2014
		F2014F2013 <- quantile(d$F2014F2013, na.rm=T,med)	    #F2014/F2013
		B2015BMSY	<- quantile(d$B2015BMSY, na.rm=T,med)	  #B2015/BMSY
		B201508BMSY	<- quantile(d$B201508BMSY, na.rm=T,med)	  #B2015/08BMSY
		B201504BMSY	<- quantile(d$B201504BMSY, na.rm=T,med)	  #B2015/04BMSY
		F2014FMSY <- quantile(d$F2014FMSY, na.rm=T,med)		  #F2014/FMSY
		B2015Bmin <- quantile(d$B2015Bmin, na.rm=T,med)		#B2015/B1971
		B2015BAvg_S <- quantile(d$B2015BAvg_S, na.rm=T,med)	#B2015/BAvg 1956-2004
		B2015BAvg_L <- quantile(d$B2015BAvg_L, na.rm=T,med)	  #B2015/BAvg 1956-2012
		F2014FAvg_S <- quantile(d$F2014FAvg_S, na.rm=T,med)	#F2014/FAvg 1956-2004
		F2014FAvg_L <- quantile(d$F2014FAvg_L, na.rm=T,med)	  #F2014/FAvg 1956-2012
	 	
		#Quantiles
		B2015q <- round(quantile(d$B2015, na.rm=T,quan),1)	  #Biomass in 2015
 		F2014q  <- round(quantile(d$F2014, na.rm=T,quan),3)	 #F in 2014  #print(round(c(i, fspr), 2))
		
		Perf_med <- rbind(Perf_med, c(i,B2015,F2014,B2015B2014,F2014F2013,B2015BMSY,B201508BMSY,B201504BMSY,F2014FMSY,B2015Bmin,B2015BAvg_S,B2015BAvg_L,F2014FAvg_S,F2014FAvg_L))
		B2015_quan <-rbind(B2015_quan,c(i,B2015q))
		F2014_quan <-rbind(F2014_quan,c(i,F2014q))
	}
			
	colnames(Perf_med )<-c("TAC","B2015","F2014","B2015B2014","F2014F2013","B2015BMSY","B201508BMSY","B201504BMSY","F2014FMSY","B2015Bmin","B2015BAvg_S","B2015BAvg_L","F2014FAvg_S","F2014FAvg_L")
        b2015name <- colnames(B2015_quan)
	b2015name[1]<-"TAC"
       colnames(B2015_quan)<-b2015name

       f2014name <- colnames(F2014_quan)
       f2014name[1]<-"TAC"
       colnames(F2014_quan)<-f2014name
      
      write.table(Perf_med, file=paste(tabDir, "Pcod_Median_Peformance_Measures.csv", sep=""), sep=",", row.names=F)
      write.table(B2015_quan, file=paste(tabDir, "Pcod_B2015_Quantiles.csv", sep=""), sep=",", row.names=F)
      write.table(F2014_quan, file=paste(tabDir, "Pcod_F2014_Quantiles.csv", sep=""), sep=",", row.names=F)
      write.table(CtlPts_quan, file=paste(tabDir, "Pcod_Control_Points_Quantiles.csv", sep=""), sep=",", row.names=F)
      
    
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#This table calculates probabilities of performance measures based on the posterior samples of performance measure quantities in pbs_pcod2013ddmcmc.proj
#	 For example: A performance measure is B2015/0.8Bmsy. A posterior sample of length 1000 will have 1000 estimates of B2015/0.8Bmsy
#       We are interested in P(B2015 < 0.8Bmsy, i.e. B2015/0.8Bmsy <1
#	Therefore calculate the proportion of posterior samples for B2015/0.8Bmsy that are less than 1
table.decision <- function(){
	dd <- A$mcproj 
	tac <- unique(dd$tac)
		
       	#Values that change with TAC
	dtable <- NULL
	
	#Values that change with TAC
	for(i in tac){
		d <- subset(dd, tac==i)
		nsamp <- length(d[,1])
		d<-d[(Burn+1):nsamp,]	  #Remove burn-in samples once subset has been done
		nd<-length(d[,1])
		#print(nsamp)
		#print(nd)
		 #print(length(d[,1]))

		#Biomass-based performance measures
		#Want probability B2015 <  the control point  (B2014, Bmsy or one of the Bavgs)
		#When  B2015 < ctl, the performance measures below are <1
		P_B2015B2014   <- length(which(d$B2015B2014<1))/nd	  #B2015/B2014
		P_B2015BMSY	<- length(which(d$B2015BMSY<1))/nd	  #B2015/BMSY
		P_B201508BMSY	<- length(which(d$B201508BMSY<1))/nd	  #B2015/08BMSY
		P_B201504BMSY	<- length(which(d$B201504BMSY<1))/nd	  #B2015/04BMSY
		P_B2015Bmin <- length(which(d$B2015Bmin<1))/nd		 #B2015/B1971
	   	P_B2015BAvg_S <- length(which(d$B2015BAvg_S<1))/nd	 #B2015/BAvg 1956-2004
	   	P_B2015BAvg_L <- length(which(d$B2015BAvg_L<1))/nd	  #B2015/BAvg 1956-2012

		#Fishing mortality-based performance measures
		#Want probability F2014 >  the control point  (F2013, Fmsy or one of the Favgs)
		#When  F2015 > ctl, the performance measures below are >1
		P_F2014F2013 <- length(which(d$F2014F2013>1))/nd	    #F2014/F2013
		P_F2014FMSY <- length(which(d$F2014FMSY>1))/nd	    #F2014/FMSY
		P_F2014FAvg_S <- length(which(d$F2014FAvg_S>1))/nd  #F2014/FAvg 1956-2004
		P_F2014FAvg_L <- length(which(d$F2014FAvg_L>1))/nd  #F2014/FAvg 1956-2012
		
		dtable <- rbind(dtable, c(i,P_B2015B2014,P_F2014F2013,P_B2015BMSY,P_B201508BMSY,P_B201504BMSY,P_F2014FMSY,P_B2015Bmin,P_B2015BAvg_S,P_B2015BAvg_L,P_F2014FAvg_S,P_F2014FAvg_L))
		
	}
			
	colnames(dtable )<-c("TAC","P_B2015B2014","P_F2014F2013","P_B2015BMSY","P_B201508BMSY","P_B201504BMSY","P_F2014FMSY","P_B2015Bmin","P_B2015BAvg_S","P_B2015BAvg_L","P_F2014FAvg_S","P_F2014FAvg_L")
              
      write.table(dtable, file=paste(tabDir, "Pcod_Decision_Table_Probs.csv", sep=""), sep=",", row.names=F)
         
}
