#**********************************************************************************
# iscam-gui-figures-selex.r
# This file contains the code for plotting selectivity values SS outputs using the
# infrastructure provided with ss-explore.
#
# Author            : Chris Grandin
# Development Date  : October 2013 - February 2015
# Current version   : 1.0
#**********************************************************************************

plotSelex <- function(scenario   = 1,            # Scenario number
                      plotNum    = 1,            # Plot code number
                      savefig    = .SAVEFIG,     # TRUE/FALSE for PNG image output
                      fileText   = "Default",    # Name of the file if png==TRUE
                      plotMCMC   = FALSE,        # TRUE/FALSE to plot MCMC output
                      ci         = NULL,         # confidence interval in % (0-100)
                      multiple   = FALSE,        # TRUE/FALSE to plot sensitivity cases
                      sensGroup  = 1,            # Sensitivity group to plot if multiple==TRUE
                      index      = 1,            # Gear index to plot
                      # PlotSpecs: Width, height, and resolution of screen and file
                      ps         = list(pngres = .RESOLUTION,
                                        pngw   = .WIDTH,
                                        pngh   = .HEIGHT,
                                        res    = .RESOLUTION,
                                        w      = .WIDTH,
                                        h      = .HEIGHT),
                      leg        = "topright",   # Legend location. If NULL, none will be drawn
                      figtype    = .FIGURE_TYPE, # The filetype of the figure with period, e.g. ".png"
                      showtitle  = TRUE,         # Show the main title on the plot
                      units      = .UNITS,       # Units to use in plotting
                      silent     = .SILENT
                      ){

  # plotNum must be one of:
  # 1  Logistic selectivity - age or length based will be detected automatically

  currFuncName <- getCurrFunc()

  if(plotNum != 1){
    return(FALSE)
  }
  scenarioName <- op[[scenario]]$names$scenario

  if(multiple){
    # Extract models in the current sensitivity group
    if(is.null(sens[[sensGroup]])){
      cat0(.PROJECT_NAME,"->",currFuncName,"The sensitivity group you selected has no members.")
      return(NULL)
    }
    models <- sens[[sensGroup]]
  }else{
    models <- scenario # For the non-multiple and retro cases
  }
  if(plotMCMC){
    # Remove models which do not have MCMC outputs
    type <- "mcmc"
    validModels <- getValidModelsList(models, type = type)
  }else{
    type <- "mpd"
    validModels <- getValidModelsList(models, type = type)
  }

  out    <- validModels[[1]]
  colors <- validModels[[2]]
  names  <- validModels[[3]]
  inputs <- validModels[[4]]
  linetypes <- validModels[[5]]
  parout <- validModels[[6]]
  controlinputs <- validModels[[7]]

  if(is.null(validModels)){
    if(is.null(names)){
      cat0(.PROJECT_NAME,"->",currFuncName,"The model ",scenarioName," has no ",type," output associated with it.\n")
    }else{
      cat0(.PROJECT_NAME,"->",currFuncName,"The model ",names[[1]]," has no ",type," output associated with it.\n")
    }
    return(NULL)
  }

  figDir       <- op[[scenario]]$names$figDir
  res          <- ps$pngres
  width        <- ps$pngw
  height       <- ps$pngh
  resScreen    <- ps$res
  widthScreen  <- ps$w
  heightScreen <- ps$h

  if(multiple){
    filenameRaw  <- paste0("SensitivityGroup_",sensGroup,"_",fileText,figtype)
    filename     <- file.path(.SENS_FIGURES_DIR_NAME,filenameRaw)
  }else{
    filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,figtype)
    filename     <- file.path(figDir,filenameRaw)
  }

  if(savefig){
    graphics.off()
    if(figtype == .PNG_TYPE){
      png(filename,res=res,width=width,height=height,units=units)
    }
    if(figtype == .EPS_TYPE){
      postscript(filename, horizontal=FALSE, paper="special",width=width,height=height)
    }
  }else{
    windows(width=widthScreen,height=heightScreen)
  }

  if(plotNum==1){
    plotLogisticSel(scenario, out, colors, names, lty = linetypes, inputs = inputs,
                    controlinputs = controlinputs, index = index, verbose = !silent, leg = leg, showtitle = showtitle)
  }
  if(plotNum>=2)  cat("No Plot Yet -- Coming Soon!!\n")

  if(savefig){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

tile <- function(){
  # Test the 'insert list element' (ile) function
  currFuncName <- getCurrFunc()

  l <- list(2,3,4)
  ind <- 1
  val <- 1
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Case 1, insert value at beginning of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,3)
  ind <- 4
  val <- 4
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Case 2, insert value at end of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,4)
  ind <- 3
  val<- 3
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Case 3, insert value in middle of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(4,5,6)
  ind <- 1
  val <- list(1,2,3)
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Case 4, insert list elements at beginning of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,3)
  ind <- 4
  val <- list(4,5,6)
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Case 5, insert list elements at end of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,6)
  ind <- 3
  val <- list(3,4,5)
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Case 6, insert list elements in middle of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

}

ile <- function(l, ind, val){
  # insert the element 'val' at list 'l' in position given by 'ind'
  # while preserving the rest of the list.
  # i.e. a list of [[1]] 1 [[2]] 2 [[3]] 4
  # with function call(l, 3, 3) will return:
  # [[1]] 1 [[2]] 2 [[3]] 3 [[4]] 4
  # Algorithm: Get the left part of the list, then glue on the 'val'
  #            element and then the right part of the list
  # if 'val' is a list, it will be inserted as if each element is
  # on its own, i.e. the return list will be a single, simple list
  # with the sublist 'val' flattened and inserted element-by-element
  #
  # Returns NA if there is an error
  # Only works on lists of values, not lists of lists.
  currFuncName <- getCurrFunc()

  if(ind < 1 || ind > (length(l)+ 1)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Index less than zero or greater than the length of the list.")
    return(NA)
  }
  # tmpl is the left part of the list
  if(ind == 1){
    tmpl <- NULL
  }else{
    tmpl <- l[1:(ind-1)]
  }
  # Glue on the 'val' element to the end of tmpl
  # remember the old index, so that we can refer to the list 'l' after
  origind <- ind
  if(is.list(val)){
    unval <- unlist(val)
    for(i in 1:length(val)){
      tmpl[[ind]] <- unval[i]
      ind <- ind + 1
    }
  }else{
    tmpl[[ind]] <- val
    ind <- ind + 1
  }
  # Glue on the right part of the list to tmpl if
  # the list has more elements to be appended
  if(origind <= length(l)){
    for(i in origind:length(l)){
      tmpl[[ind]] <- l[[i]]
      ind <- ind + 1
    }
  }
  return(as.list(tmpl))
}

plotLogisticSel	<-	function(scenario, out, colors, names, lty, inputs, controlinputs, index, verbose, leg, showtitle = TRUE){
  # Currently only implemented for seltypes 1,6 and 11 (estimated logistic age-based, fixed logistic age-based, or estimated logistic length-based)
  # Both sexes will be plotted with linetype of the females = linetype for males + 1 The colors will be the same.
  # Notes:
  # - Models may have different gears than others, but we want the selectivity plots to match by gear.
  #   The solution is to match them by name if plotting multiple (sensitivity plots)
  #   by creating a unique vector of names which is the union of all names across all models
  #   and using that to match to the names in each model, only plotting if the name is found.
  #
  # - Selectivity blocks (if more than one) will be drawn with the same color as the scenario's color,
  #   but incrementing line tyles (lty) and labelled on the legend with the range of years the block covers.

  currFuncName <- getCurrFunc()
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  # Get a list of unique index names across all models to be included in this plot
  agegearnames <- NULL
  for(model in 1:length(inputs)){
    agegearnames <- c(agegearnames, inputs[[model]]$ageGearNames)
  }
  if(is.null(agegearnames)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply age gear names in the data files to plot selectivities across models.")
    return(NULL)
  }
  agegearnames <- unique(agegearnames)
  if(index > length(agegearnames)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply a gear number less or equal to ",length(agegearnames),".")
    return(NULL)
  }
  curragegearname <- agegearnames[index]
  # The 'agegearnames' vector will be used as the gear to scroll through,
  # i.e. when the user changes to the next gear, the next name in this list will
  # be matched.
  titleText    <- agegearnames[index]

  # mat wiull hold all rows to be plotted. These can be different models, multiple time blocks
  # within a model, two sexes, or any combination of these.
  # lty, names, and colors will also be modified to reflect the complexities stated.
  mat <- NULL
  for(model in 1:length(out)){
    age <- out[[model]]$mpd$age
    gearnum      <- match(curragegearname, inputs[[model]]$ageGearNames)
    logselData   <- out[[model]]$mpd$log_sel
    if(is.na(gearnum)){
      # Remove the gear from the legend lists by setting to NA
      lty[[model]] <- NA
      colors[[model]] <- NA
      names[[model]] <- NA
    }else{
      # Get the selectivity time blocks for this gear (index)
      tb           <- controlinputs[[model]]$syrtimeblock[gearnum,]
      nsex         <- inputs[[model]]$nsex
      age          <- out[[model]]$mpd$age
      agegearnames <- inputs[[model]]$ageGearNames
      logselData   <- logselData[which(logselData[,1] == gearnum),]
      # Cannot use index here because it may not match what is on the UI
      #nb <- controlinputs[[model]]$sel["nselblocks",][index]
      nb <- controlinputs[[model]]$sel["nselblocks",][gearnum]
      yrs <- logselData[,3]
      if(nb > 1){
        # Remove the current legend parameters, since we are going to replace them
        # with multiple blocks
        lty1 <- lty[[model]]
        colors1 <- colors[[model]]
        names1 <- names[[model]]
        lty[[model]] <- NA
        colors[[model]] <- NA
        names[[model]] <- NA
        # The following loop slices the block data into the correct blocks by year,
        # i.e. if vector tb = 1996 2004 will be translated to 1996-2003
        #      and 2004-end year of data
        for(nblock in 1:nb){
          if(nb == nblock){
            endyr <- yrs[length(yrs)]
          }else{
            endyr <- tb[nblock+1] - 1
          }
#browser()          
          if(endyr > tb[nblock]){
            dat <- logselData[logselData[,3] >= tb[nblock] & logselData[,3] < endyr,]
            # place data in the matrix 'mat' and modify legend parameters
            selData <- exp(dat[,4:ncol(dat)])
            selData <- selData[nrow(selData),] # end-year selectivity for this block
            mat <- cbind(mat, selData)
            # Modify legend parameters to show selectivity blocks
            lty[[length(lty) + 1]] <- lty1
            lty1 <- lty1 + 1
            colors[[length(colors) + 1]] <- colors1
            names[[length(names) + 1]] <- paste0(names1, " - ",tb[nblock],"-",endyr)
          }
        }
      }else{
        if(nsex == 2){
          # There is no sex-specific selectivity, but we need to extract one of them
          # since two sexes are reported. May as well choose Female.
          logselData <- logselData[which(logselData[,2] == 2),]
        }
        selData <- exp(logselData[,4:ncol(logselData)])
        selData <- selData[nrow(selData),] # end-year selectivity for the only block
        #selData <- as.matrix(selData)
        mat <- cbind(mat, selData)
      }
      gearTitle <- agegearnames[gearnum]
    }
  }
  titletext <- ""
  if(showtitle){
    titletext <- gearTitle
  }
  # Change the NAs to NULLs for the legend variables, using the property that if a list
  # element is set to NULL, it will be removed completely from the list.
  lty[sapply(lty, is.na)] <- NULL
  colors[sapply(colors, is.na)] <- NULL
  names[sapply(names, is.na)] <- NULL
  matplot(age, mat, type = "l", lwd = 2, lty = unlist(lty), col = unlist(colors), las = 1, main = titletext, ylim = c(0,1.1), ylab="", xlab="Age")
#  if(selType != 1 && selType != 6 && selType != 11){
#    cat0(.PROJECT_NAME,"->",currFuncName,"The selectivity plotting function can only plot logistic selectivity for age or length (types 1,6,11 only).")
#    return(NULL)
#  }
# lines(age, selData[,1], type="l", lwd=2, lty=lty[[model]], col=colors[[model]], las=1, main=gearTitle, ylim=c(0,1.1))
  if(!is.null(leg)){
    legend(leg, legend=names, col=unlist(colors), lty=unlist(lty), lwd=2)
  }
}
