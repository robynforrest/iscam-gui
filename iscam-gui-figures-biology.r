#**********************************************************************************
# ss-explore-figures-biology.r
# This file contains the code for plotting biological values iScam outputs.
#
# Author            : Chris Grandin
# Development Date  : August 2013 - Present
# Current version   : 1.0
#**********************************************************************************

plotBiology <- function(plotNum    = 1,         # Plot code number
                        compFitSex = 1,         # Composition plots sex to plot (1=M/Both, 2=F)
                        png        = .PNG,      # TRUE/FALSE for PNG image output
                        fileText   = "Default", # Name of the file if png==TRUE
                        plotMCMC   = FALSE,     # TRUE/FALSE to plot MCMC output
                        ci         = NULL,      # confidence interval in % (0-100)
                        multiple   = FALSE,     # TRUE/FALSE to plot sensitivity cases
                        sensGroup  = 1,         # Sensitivity group to plot if multiple==TRUE
                        index      = 1,         # Index/gear to plot
                        # PlotSpecs: Width, height, and resolution of screen and file
                        ps         = list(pngres = .RESOLUTION,
                                          pngw   = .WIDTH,
                                          pngh   = .HEIGHT,
                                          res    = .RESOLUTION,
                                          w      = .WIDTH,
                                          h      = .HEIGHT),
                        leg        = "topright", # Legend location. If NULL, none will be drawn
                        units      = .UNITS){

  # plotNum must be one of:
  # 1  Mean weight at length for last year
  # 2  Maturity at age
  # 3  Fecundity (only plotted if  all elements of biology$Fecundity are not NA)
  # 4  Fecundity as a function of weight (not always available)
  # 5  Fecundity as a function of length (not always available)
  # 6  Spawning output at length
  # 7  Ending year expected growth, length at age
  # 8  Time-varying natural mortality (only plotted if growthdatF$M vector contains different values)
  # 9  Perspective plot of time-varying growth (only plotted if growthvaries and growthseries are non-NULL)
  # 10 Contour plot of time-varying growth (only plotted if growthvaries and growthseries are non-NULL)
  # 11 Bubble plot of composition data, two-paned if it is a two-sex model.
  # 12 Bar plot of composition fits
  # 13 Bubble plot of composition residuals
  # 14 LW relationship with fit a parameter estimates
  # 15 VONB relationship with fit a parameter estimates
  # 16 MA relationship with fit a parameter estimates

  if(plotNum < 1 || plotNum > 16){
    return(FALSE)
  }
  val          <- getWinVal()
  currFuncName <- getCurrFunc()
  scenario     <- val$entryScenario
  isMCMC       <- op[[scenario]]$inputs$log$isMCMC
  figDir       <- op[[scenario]]$names$figDir
  out          <- op[[scenario]]$outputs$mpd
  outMCMC      <- op[[scenario]]$outputs$mcmc
  #filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,".png")
  filenameRaw  <- paste0(fileText,".png")
  filename     <- file.path(figDir,filenameRaw)
  # PlotSpecs: Width, height, and resolution of screen and file
  res          <- ps$pngres
  width        <- ps$pngw
  height       <- ps$pngh
  resScreen    <- ps$res
  widthScreen  <- ps$w
  heightScreen <- ps$h

  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)
  }else{
    windows(width=widthScreen,height=heightScreen)
  }

  if(plotNum==1)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==2)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==3)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==4)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==5)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==6)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==7)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==8)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==9)  cat("No Plot Yet -- Coming Soon!!\n")
  if(plotNum==10) cat("No Plot Yet -- Coming Soon!!\n")

  # Composition plots
  if(plotNum==11) plotComps(1, compFitSex, scenario, index, leg)
  if(plotNum==12) plotComps(2, compFitSex, scenario, index, leg)
  if(plotNum==13) plotComps(3, compFitSex, scenario, index, leg)
  # Biological plots
  if(plotNum==14) plotLW(leg)
  if(plotNum==15) plotGrowth(leg)
  if(plotNum==16) plotMA(leg)

  if(png){
    cat0(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n")
    dev.off()
  }
  return(TRUE)
}

plotLW <- function(leg){
  # Plot the length/weight data and fit from the bio global object
  # If split sex, plot both with individual fits.
  # First column of 'data' assumed to be length in mm, second is round weight in grams.
  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  if(!exists("bio", envir = .GlobalEnv)){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - object 'bio' does not exist. Run the length/weight model from the Biotool tab.")
    return(NULL)
  }
  legNames <- NULL
  legCols <- NULL
  data <- bio$lw
  if(is.null(data)){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - element 'lw' of object 'bio' does not exist. Run the length/weight model from the Biotool tab.")
    return(NULL)
  }
  # For alpha and beta as greek letters in plots
  greek <- c("alpha", "beta")
  cnames <- paste(LETTERS[1:2], letters[1:2])
  legendExp <- sapply(1:2, function(i) {
    as.expression(substitute(A (B),
        list(A = as.name(cnames[i]), B = as.name(greek[i]))))
  })

  if(length(data) == 2){
    # Split sexes
    for(sex in 1:2){
      lw <- data[[sex]][[1]]
      l <- lw[,1]/10.0 # divide by ten to go from mm->cm
      w <- lw[,2]
      if(sex == 1){
        col <- "blue"
        shade <- .getShade(col, 20)
        plot(l, w, col=shade, pch=1, xlab="Length (cm)", ylab="Weight (g)")
      }else{
        col <- "red"
        shade <- .getShade(col, 20)
        points(l, w, col=shade, pch=1, xlab="Length (cm)", ylab="Weight (g)")
      }
      a <- data[[sex]][[2]][1,]
      b <- data[[sex]][[2]][2,]
      curve(a*x^b, col=col, lwd=3, add=T)
      legCols <- c(legCols, col)
      if(sex == 1){
        legNames <- c(legNames, as.expression(substitute(paste("Male  ", alpha, "=", a, " ", beta,"=", b, "\n"))))
      }else{
        legNames <- c(legNames, as.expression(substitute(paste("Female  ", alpha, "=", a, " ", beta,"=", b, "\n"))))
      }
    }
  }else{
    # Combined sexes
    lw <- data[[1]][[1]]
    l <- lw[,1]/10.0 # divide by ten to go from mm->cm
    w <- lw[,2]
    col <- "blue"
    plot(l, w, col=col, xlab="Length (cm)", ylab="Weight (g)")
    a <- data[[1]][[2]][1,]
    b <- data[[1]][[2]][2,]
    curve(a*x^b, col=col, lwd=3, add=T)
    legCols <- c(legCols, col)
    legNames <- c(legNames, as.expression(substitute(paste("Combined sexes  ", alpha, "=", a, " ", beta,"=", b, "\n"))))
  }
  if(!is.null(leg)){
    legend(leg, legend=legNames, col=legCols, lty=1, lwd=2)
  }
}

plotMA <- function(leg  = NULL){
  # Plot the maturity/age data and fit from the bio global object
  # If split sex, plot both with individual fits.
  # First column of 'data' assumed to be length in mm, second is maturity level.
  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  if(!exists("bio", envir = .GlobalEnv)){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - object 'bio' does not exist. Run the maturity/age model from the Biotool tab.")
    return(NULL)
  }
  legNames <- NULL
  legCols <- NULL
  data <- bio$ma
  if(is.null(data)){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - element 'ma' of object 'bio' does not exist. Run the maturity/age model from the Biotool tab.")
    return(NULL)
  }
  if(length(data) == 2){
    # Split sexes
    for(sex in 1:2){
      ma <- data[[sex]][[1]]
      a <- ma[,1]
      m <- ma[,2]
      xlim <- c(0,max(a))
      if(sex == 1){
        col <- "blue"
        shade <- .getShade(col, 80)
        plot(a, m, col=shade, pch=1, xlim=xlim, xlab="Age", ylab="Proportion mature")
      }else{
        col <- "red"
        shade <- .getShade(col, 80)
        points(a, m, col=shade, pch=1, xlab="Age", ylab="Proportion mature")
      }
      a50 <- data[[sex]][[2]][1,]
      sigma_a50 <- data[[sex]][[2]][2,]
      curve(1/(1+exp(-(x-a50)/sigma_a50)), col=col, lwd=3, add=TRUE)
      legCols <- c(legCols, col)
      if(sex == 1){
        legNames <- c(legNames, as.expression(substitute(paste("Male  a"["50%"], " = ", a50, " std"["50%"], "  = ", sigma_a50, "\n"))))
      }else{
        legNames <- c(legNames, as.expression(substitute(paste("Female  a"["50%"], " = ", a50, " std"["50%"], "  = ", sigma_a50, "\n"))))
      }
    }
  }else{
    # Combined sexes
    ma <- data[[1]][[1]]
    a <- ma[,1]
    m <- ma[,2]
    xlim <- c(0,max(a))
    col <- "blue"
    shade <- .getShade(col, 80)
    plot(a, m, col=shade, xlim=xlim, xlab="Age", ylab="Proportion mature")
    a50 <- data[[1]][[2]][1,]
    sigma_a50 <- data[[1]][[2]][2,]
    curve(1/(1+exp(-(x-a50)/sigma_a50)), col=col, lwd=3, add=TRUE)
    legCols <- c(legCols, col)
    legNames <- c(legNames, as.expression(substitute(paste("Combined sexes a"["50%"], " = ", a50, " std"["50%"], "  = ", sigma_a50, "\n"))))
  }
  if(!is.null(leg)){
    legend(leg, legend=legNames, col=legCols, lty=1, lwd=2)
  }
}

plotGrowth <- function(leg){
  # Plot the length/age data and fit from the bio global object
  # If split sex, plot both with individual fits.
  # First column of 'data' assumed to be length in mm, second is age.
  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  if(!exists("bio", envir = .GlobalEnv)){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - object 'bio' does not exist. Run the length/weight model from the Biotool tab.")
    return(NULL)
  }
  legNames <- NULL
  legCols <- NULL
  data <- bio$vonb
  if(is.null(data)){
    cat0(.PROJECT_NAME,"->",getCurrFunc(),"Error - element 'vonb' of object 'bio' does not exist. Run the VonB model from the Biotool tab.")
    return(NULL)
  }
  if(length(data) == 2){
    # Split sexes
    for(sex in 1:2){
      la <- data[[sex]][[1]]
      l  <- la[,1]/10.0 # divide by ten to go from mm->cm
      a  <- la[,2]
      if(sex == 1){
        col <- "blue"
        shade <- .getShade(col, 20)
        plot(a, l, col=shade, pch=1, xlab="Age", ylab="Length (cm)")
      }else{
        col <- "red"
        shade <- .getShade(col, 20)
        points(a, l, col=shade, pch=1, xlab="Age", ylab="Length (cm)")
      }
      linf <- data[[sex]][[2]][1,]
      k    <- data[[sex]][[2]][2,]
      tt0  <- data[[sex]][[2]][3,]
      curve(linf*(1-exp(-k*x)), col=col, lwd=3, add=T)
      legCols <- c(legCols, col)
      if(sex == 1){
        legNames <- c(legNames, as.expression(substitute(paste("Male  L"[infinity], " = ", linf, " ", kappa, " = ", k, " tt"[0], " = ", tt0, "\n"))))
      }else{
        legNames <- c(legNames, as.expression(substitute(paste("Female  L"[infinity], " = ", linf, " ", kappa, " = ", k, " tt"[0], " = ", tt0, "\n"))))
      }
    }
  }else{
    # Combined sexes
    la  <- data[[1]][[1]]
    l   <- la[,1]/10.0 # divide by ten to go from mm->cm
    a   <- la[,2]
    col <- "blue"
    plot(a, l, col=col, xlab="Age", ylab="Length (cm)")
    linf <- data[[1]][[2]][1,]
    k    <- data[[1]][[2]][2,]
    tt0  <- data[[1]][[2]][3,]
    curve(linf*(1-exp(-k*x)), col=col, lwd=3, add=T)
    legCols <- c(legCols, col)
    legNames <- c(legNames, as.expression(substitute(paste("Combined sexes  L"[infinity], " = ", linf, " ", kappa, " = ", k, " tt"[0], " = ", tt0, "\n"))))
   }
  if(!is.null(leg)){
    legend(leg, legend=legNames, col=legCols, lty=1, lwd=2)
  }
}

plotComps <- function(plotnum = 1, sex, scenario, index, leg){
  # Plot the age or length compositions for the given index (gear).
  # If the model is two-sex, a two-paneled plot will be drawn.
  # plotnum:
  # 1 = composition
  # 2 = composition fits
  # 3 = compostion residuals
  # sex 0=Combined, 1=M, 2=F

  currFuncName <- getCurrFunc()

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  nSex <- op[[scenario]]$inputs$data$nsex
  nAgears <-  op[[scenario]]$input$data$nagears
  nAgearsobs <- op[[scenario]]$input$data$nagearsvec
  # ageLengthFlags, 0 = length data 1= age data, if two-sex model this will be length 2 vector
  ageLengthFlags <- op[[scenario]]$input$data$agecompflag

  gearNames <- op[[scenario]]$inputs$data$ageGearNames
  if(op[[scenario]]$inputs$data$hasAgeGearNames){
    titleText <- gearNames[index]
  }else{
    titleText <- paste0("Gear ",gearNames[index])
  }

  if(nAgearsobs[1] > 0){
    compData <-  as.data.frame(op[[scenario]]$outputs$mpd$d3_A)
    fitData <- as.data.frame(op[[scenario]]$output$mpd$A_hat)
    residData <- as.data.frame(op[[scenario]]$output$mpd$A_nu)
    gears <- unique(compData[,2])

    if(is.element(index, gears)){
      # Get the index for the gear associated with the index number so the correct sage and nage can be extracted
      # For example, if the two gears with data are 1 and 3, when the user selects index 3 on the GUI,
      #  sage and nage are the SECOND elements of n_A_sage and n_A_nage
      gearindex <- which(gears==index)
      sage <- op[[scenario]]$output$mpd$n_A_sage[gearindex]  # Need to match the gear number to the correct element of n_A_sage
      nage <- op[[scenario]]$output$mpd$n_A_nage[gearindex]  # Need to match the gear number to the correct element of n_A_nage
      nages <- length(sage:nage)
      flag <- ageLengthFlags[gearindex]
      if(flag == 0){
        ylab="Length"
      }
      if(flag == 1){
        ylab="Age"
      }

      compData <- compData[which(compData[,2]==index) ,]   # Get only the composition data for the current index

      yrs <- compData[,1]
      iyr <- length(yrs)
      syr <- yrs[1]
      nyr <- yrs[length(yrs)]

      # Extract the data for the given sex (column 5)
      compdat <- compData[compData[,5] == sex,]
      if(length(compdat[,1]) > 0){
        yrs <- compdat[,1]
        # Remove header columns from data
        compdat <- compdat[, 6:ncol(compdat)]
        # Remove NAs from ragged array if they exist
        compdat <- compdat[, 1:nages]

        # Extract the fit data for the given sex, odd or even rows.
        if(sex > 0){
          fitdat <- fitData[seq(sex,nrow(fitData),2),]
        }else{
          fitdat <- fitData
        }
        # Get row proportions from composition data
        prop <- apply(compdat, 1, function(x){x/sum(x)})
        if(sex == 1){
          sexstr <- "Male"
        }else if(sex == 2){
          sexstr <- "Female"
        }else{
          sexstr <- "Combined sexes"
        }
        if(plotnum == 1){
          plotCompositions(prop, yrs, sage:nage, sexstr, titleText, leg, ylab)
        }
        if(plotnum == 2){
          plotCompositionsFit(t(prop), fitdat, yrs, sage:nage, sex, sexstr, titleText, leg, ylab)
        }
        if(plotnum == 3){
          plotCompositionsResids()
        }
        #title(titleText, outer=TRUE)
      }
    }else{
      cat0(.PROJECT_NAME,"->",currFuncName,"No composition data for this gear.")
    }
  }else{
    cat0(.PROJECT_NAME,"->",currFuncName,"No composition data for this scenario.")
  }
}

plotCompositions <- function(prop, yrs, ages, title, gearTitle, leg,  ylab, size = 0.1, powr = 0.5,
                             las = 1, leglabels = c("Positive","Zero"),
                             col = c("black","blue"), pch = 1, bty = "n", cex = 1.25, titleText){
  # Plot the age or length compositions given in prop
  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  plotBubbles(prop, xval=yrs, yval=ages, prettyaxis=TRUE, size=0.1, powr=0.5,
              xlab="Year", main=paste0(gearTitle," - ",title), ylab=ylab, las=las, xaxt="n")
  axis(1, at=yrs, labels=yrs, las=2)
  legend(leg, legend=leglabels, col=col, pch=pch, bty=bty, cex=cex)
}

plotCompositionsFit <- function(prop, fit, yrs, ages, sex, title, gearTitle, leg,  ylab, size = 0.1, powr = 0.5,
                                las = 1, leglabels = c("Positive","Zero"),
                                col = c("black","blue"), pch = 1, bty = "n", cex = 1.25, titleText){
  # Plot the age or length composition fits
  # Set the number of panels per plot - no more than 16 per page
  # sex, 1=M/Both, 2=F

  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  if(nrow(prop) <= 16) par(mfrow=c(4,4), oma=c(2,3,1,1), mai=c(0.3,0.3,0.3,0.2))
  if(nrow(prop) <=  4) par(mfrow=c(2,2), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2))
  if(nrow(prop) >  16) par(mfrow=c(4,4), oma=c(2,3,1,1), mai=c(0.2,0.2,0.2,0.2))
  plotCount <- 1
  for(yr in 1:length(yrs)){
    year <- yrs[yr]
    obs  <- prop[yr,]
    est  <- fit[yr,]
    plot(ages, obs, type="h", xlab="", ylab="", main=paste(year), las=1, ylim=c(0,max(rbind(obs,est))))
    lines(ages, est, lty=1, lwd=2, col=2)
    if(plotCount==1){
      legend(leg, legend=c("Obs", "Est"), lty=1, lwd=2, col=1:2, bty="n")
    }
    plotCount <- plotCount + 1
    if(plotCount == 16){
      mtext(paste(ylab), side=1, line=0.5, cex=1.3, outer=TRUE)
      mtext("Proportion", side=2, line=0.6, cex=1.3, outer=TRUE)
      mtext(paste0(gearTitle," - ",title), side=3, line=-0.5, cex=1.3, outer=FALSE)
      #par(mfrow=c(4,4), oma=c(2,3,1,1), mai=c(0.3,0.3,0.3,0.2))
      plotCount <- 1
    }
    if(yr == length(yrs)){
      mtext(paste(ylab), side=1, line=0.5, cex=1.3, outer=TRUE)
      mtext("Proportion", side=2, line=0.6, cex=1.3, outer=TRUE)
      mtext(paste0(gearTitle," - ",title), side=3, line=-0.5, cex=1.3, outer=TRUE)
    }
  }
}

plotCompositionsResids1 <- function(scenario, index, leg){
  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  nAgears <- op[[scenario]]$input$data$nagears
  nAgearsobs <- op[[scenario]]$input$data$nagearsvec
  ageLengthFlags <- op[[scenario]]$input$data$agecompflag  #0 = length data 1= age data

  gearNames <- op[[scenario]]$inputs$data$ageGearNames
  if(op[[scenario]]$inputs$data$hasAgeGearNames){
    titleText <- gearNames[index]
  }else{
    titleText <- paste0("Gear ",gearNames[index])
  }

  if(nAgearsobs[1] > 0){
    compData <-  as.data.frame(op[[scenario]]$output$mpd$d3_A ) #Get the composition data - need this because there is no gear information with the residuals  
    residData  <-  as.data.frame(op[[scenario]]$output$mpd$A_nu)  #Get the residual data
    gears <- unique(compData[,2])
    if(is.element(index, gears)){
      #Get the index for the gear associated with the index number so the correct sage and nage can be extracted
      #For example, if the two gears with data are 1 and 3, when the user selects index 3 on the GUI, sage and nage are the SECOND elements of n_A_sage and n_A_nage
      gearindex <- which(gears==index)
      sage <- op[[scenario]]$output$mpd$n_A_sage[gearindex]  #Need to match the gear number to the correct element of n_A_sage
      nage <- op[[scenario]]$output$mpd$n_A_nage[gearindex]  #Need to match the gear number to the correct element of n_A_nage
      flag <- ageLengthFlags[gearindex]
      if(flag==0){
        ylab="Length"
      }
      if(flag==1){
        ylab="Age"
      }

      compData <- as.data.frame(compData[which(compData[,2]==index) ,] )  #Get only the composition data for the current index
      yrs <- compData[,1]
      syr <- yrs[1]
      iyr <- length(yrs)
      nyr <- yrs[iyr]
      residData <- residData[which(compData[,2]==index) ,]   #Use the composition dataframe to get the right rowsfor the residuals (i.e.,for the current index)
      plotBubbles(t(residData), xval=yrs,yval=sage:nage, prettyaxis=T, size=0.1,powr=0.5,xlab="Year",
                  ylab=ylab,main=titleText, las=1, cex.axis=0.75)
      legend(leg, legend=c("Positive", "Negative", "Zero"), col=c("black","red","blue"), pch=1, bty="n", cex=1.25)
    }else{
          cat0(.PROJECT_NAME,"->",currFuncName,"No composition data for this gear.")
    }
  }else{
        cat0(.PROJECT_NAME,"->",currFuncName,"No composition data for this scenario.")
  }
}
