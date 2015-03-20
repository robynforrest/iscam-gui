.getShade <- function(color,opacity){
  # If color is a single R color string or single number,
  #  returns an rgb string of the specified color and opacity
  # If color is a vector of cR color strings or numbers,
  #  returns a vector of rgb strings of the specified color and opacity.
  # If the opacity argument is non-integer or not between 0 and 99, NULL will be returned.
  # - opacity - 2-decimal-digit string (00-99), i.e. "20" means 20%
  # Notes: format of returned string is #RRGGBBAA
  #        where RR=red, a 2-hexadecimal-digit string
  #        GG=green, a 2-hexadecimal-digit string
  #        BB=blue, a 2-hexadecimal-digit string
  #        AA=alpha or opacity
  #
  # The opacity agrument is scalar and will be applied to all colors.
  if(!(opacity %% 1 == 0) || opacity<0 || opacity>99){
    cat(getCurrFunc(),"opacity argument must be an integer between 0 and 99.\n",sep="")
    return(NULL)
  }
  colorDEC <- col2rgb(color)
  if(is.matrix(colorDEC)){
    colorHEX <- matrix(nrow=3,ncol=ncol(colorDEC))
    shade <- NULL
    for(col in 1:ncol(colorDEC)){
      for(row in 1:nrow(colorDEC)){
        colorHEX[row,col] <- sprintf("%X", colorDEC[row,col])
        if(nchar(colorHEX[row,col])==1){
          colorHEX[row,col] <- paste0("0",colorHEX[row,col])
        }
      }
      shade[col] <- paste0("#",colorHEX[1,col],colorHEX[2,col],colorHEX[3,col],opacity)
    }
  }else{
    colorHEX <- sprintf("%X", colorDEC)
    for(i in 1:length(colorHEX)){
      if(nchar(colorHEX[i])==1){
        colorHEX[i] <- paste0("0",colorHEX[i])
      }
    }
    shade <- paste0("#",colorHEX[1],colorHEX[2],colorHEX[3],opacity)
  }
  return(shade)
}

.gletter <- function(letter){
  # gletter()
  # adds letters to plot panels
  # - letter - the letter to place on the panel
  usr <- par("usr")
  inset.x <- 0.05*(usr[2]-usr[1])
  inset.y <- 0.05*(usr[4]-usr[3])
  text(usr[1]+inset.x,usr[4]-inset.y,paste("(",letters[letter],")",sep=""),cex=1.,font=1)
}

curfnfinder <- function(skipframes=0, skipnames="(FUN)|(.+apply)|(replicate)",
    retIfNone="Not in function", retStack=FALSE, extraPrefPerLevel="\t")
{
  # Get the current function name from within the function itself.
  # Used to prepend the function name to all messages so that the
  # user knows where the message came from.
    prefix<-sapply(3 + skipframes+1:sys.nframe(), function(i){
            currv<-sys.call(sys.parent(n=i))[[1]]
            return(currv)
        })
    prefix[grep(skipnames, prefix)] <- NULL
    prefix<-gsub("function \\(.*", "do.call", prefix)
    if(length(prefix)==0)
    {
        return(retIfNone)
    }
    else if(retStack)
    {
        return(paste(rev(prefix), collapse = "|"))
    }
    else
    {
        retval<-as.character(unlist(prefix[1]))
        if(length(prefix) > 1)
        {
            retval<-paste(paste(rep(extraPrefPerLevel, length(prefix) - 1), collapse=""), retval, sep="")
        }
        return(retval)
    }
}

catw <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
    append = FALSE, prefix=0)
{
  # writes out some innformation on the calling function to screen
    if(is.numeric(prefix))
    {
        prefix<-curfnfinder(skipframes=prefix+1) #note: the +1 is there to avoid returning catw itself
        prefix<-paste(prefix, ": ", sep="")
    }
    cat(prefix, ..., format(Sys.time(), "(%Y-%m-%d %H:%M:%S)"), "\n",
        file = file, sep = sep, fill = fill, labels = labels, append = append)
}

getCurrFunc <- function(){
  # Returns the calling function's name followed by ": "
  funcName <- curfnfinder(skipframes=1) # skipframes=1 is there to avoid returning getCurrFunc itself
  # Strip extraneous whitespace
  funcName <- gsub("\t+","",funcName)
  funcName <- gsub("\ +","",funcName)
  funcName <- paste(funcName,": ",sep="")
  return(funcName)
}

cat0 <- function(...){
  cat(..., "\n", sep="")
}

addpoly <- function(yrvec, lower, upper, col, shadeCol, lty = 3){
  # add shaded uncertainty polygon around time series data
  # yrvec is a vector of time series data
  # lower is the lower line of the polygon
  # upper is the upper line of the polygon
  # col is the outline color for the polygon
  # shadeCol is the shade color for the polygon
  lower[lower<0] <- 0 # max of value or 0
  polygon(x = c(yrvec,rev(yrvec)),
          y = c(lower,rev(upper)),
          border = col,
          col = shadeCol)
  lines(yrvec, lower, lty=lty, col=col)
  lines(yrvec, upper, lty=lty, col=col)
}

getQuants <- function(data=NULL, ci=NULL){
  # Return the column quantiles for data matrix.
  # The median along with the confidence interval 'ci'
  # will be calculated and the quantiles returned.
  currFuncName <- getCurrFunc()
  if(is.null(data)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an input posterior matrix (data).")
    return(NULL)
  }
  if(is.null(ci)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an input confidence interval in % (ci).")
    return(NULL)
  }
  ciprop <- ci / 100
  probs <- c((1-ciprop)/2,0.5,1-((1-ciprop)/2))
  if(is.null(dim(data))){
    # It is a single posterior, e.g. sbo
    quants <- quantile(data, probs)
  }else{
    # It is a timeseries posterior, e.g. sbt
    quants <- apply(data, 2, quantile, probs)
  }
  return(quants)
}

drawEnvelope <- function(yrs, quants, color, yLower = 0, yUpper, first, ...){
  # Draw a time series envelope on a device on which plot.new has already been called
  # Assumptions: quants is a 3-row matrix,
  #  where the middle row is the median and the other two are the lower and upper
  #  values for some confidence interval.
  # yUpper is the upper limit for the y-axis
  # first is a boolean, if TRUE, plot will be called. If FALSE, lines will be called.
  lower  <- quants[1,]
  median <- quants[2,]
  upper  <- quants[3,]

  if(first){
    plot(yrs, median, type="l", col=color, lty=1, lwd=2, ylim=c(yLower, yUpper), ...)
  }else{
    lines(yrs, median, type="l", col=color, lty=1, lwd=2, ylim=c(yLower, yUpper), ...)
  }

  shade <- .getShade(color, 30)
  polyYears <- c(yrs, rev(yrs))
  polyCI    <- c(lower, rev(upper))
  polygon(polyYears, polyCI, col = shade)
}

getValidModelsList <- function(models, retros = FALSE, type = "mpd"){
  # Return a list of data, colors, linetypes, names, and inputs for the given set of models,
  # for type mcmc or mpd (must be lower case).
  # If retros==TRUE, everything in the $outputs$retros list will be added,
  #  and the first element of 'models' will be used as the base.
  # Only models which have been run in the given mode will be returned.

  currFuncName <- getCurrFunc()
  if(retros){
    retroScenario <- models[1]
    numRetros <- length(op[[retroScenario]]$outputs$retros) + 1  # +1 for the base; numRetros includes base run
    hasType <- vector("numeric", length = numRetros)
    for(model in 1:numRetros){
      if(model == 1){
        # base model
        #hasType[[model]] <- !is.null(unlist(op[[models[model]]]$outputs[type]))
        hasType[[model]] <- !is.null(unlist(op[[retroScenario]]$outputs[type]))
      }else{
        # retrospectives
        hasType[[model]] <- !is.null(unlist(op[[retroScenario]]$outputs$retros[[model-1]]$outputs[type]))
      }
    }
  }else{
    hasType <- vector("numeric", length = length(models))
    for(model in 1:length(models)){
      hasType[[model]] <- !is.null(unlist(op[[models[model]]]$outputs[type]))
    }
  }
  if(hasType == 0){
    cat0(.PROJECT_NAME,"->",currFuncName,"There are no models which have been run in '",type,"' mode. No plot to draw.")
    return(NULL)
  }
  inputs <- out <- colors <- linetypes <- names <- parout <- controlinputs <- vector("list", len <- sum(hasType))
  if(retros){
    # models and nonmodels refer to the base followed by the retrospectives
    nonmodels <- !hasType
    models <- hasType
  }else{
    nonmodels <- models[hasType == 0]
    models <- models[hasType == 1]
    if(length(nonmodels) > 0){
      for(model in 1:length(nonmodels)){
        cat0(.PROJECT_NAME,"->",currFuncName,"Model name ",op[[nonmodels[model]]]$names$scenario,
             " has not been run in ",type," mode and cannot be plotted.")
      }
    }
  }
  for(model in 1:len){
    if(retros){
      if(model == 1){
        out[[model]] <- op[[retroScenario]]$outputs[type]
        names[[model]]  <- op[[retroScenario]]$names$scenario
        inputs[[model]] <- op[[retroScenario]]$inputs$data
        inputs[[model]]$sel <- op[[retroScenario]]$inputs$control$sel
        linetypes[[model]] <- op[[models[1]]]$inputs$linetype
        parout[[model]] <- op[[retroScenario]]$outputs$par
        controlinputs[[model]] <- op[[retroScenario]]$inputs$control
      }else{
        out[[model]]    <- op[[retroScenario]]$outputs$retros[[model-1]]$outputs[type]
        names[[model]]  <- op[[retroScenario]]$outputs$retros[[model-1]]$names$scenario
        inputs[[model]] <- op[[retroScenario]]$outputs$retros[[model-1]]$inputs$data
        inputs[[model]]$sel <- op[[retroScenario]]$outputs$retros[[model-1]]$inputs$control$sel
        linetypes[[model]] <- op[[retroScenario]]$outputs$retros[[model-1]]$inputs$linetype
        parout[[model]] <- op[[retroScenario]]$outputs$par
        controlinputs[[model]] <- op[[retroScenario]]$inputs$control
      }
      colors[[model]] <- .RETRO_COLORS[model]
    }else{
      out[[model]]    <- op[[models[model]]]$outputs[type]
      colors[[model]] <- op[[models[model]]]$inputs$color
      names[[model]]  <- op[[models[model]]]$names$scenario
      inputs[[model]] <- op[[models[model]]]$inputs$data
      inputs[[model]]$sel <- op[[models[model]]]$inputs$control$sel
      linetypes[[model]] <- op[[models[model]]]$inputs$linetype
      parout[[model]] <- op[[models[model]]]$outputs$par
      controlinputs[[model]] <- op[[models[model]]]$inputs$control
    }
  }
  if(length(out) == 1 && is.null(out[[1]][[1]])){
    return(NULL)
  }
  ret <- list(out,colors,names,inputs,linetypes,parout,controlinputs)
  return(ret)
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  # Return TRUE if x is an integer, FALSE otherwise
  abs(x - round(x)) < tol
}

getRowsCols <- function(num){
  # Returns a vector of length 2 representing the number of
  # rows and columns to use to pack a plot in a grid
  if(num <= 49 && num > 36){
    if(num <= 42){
      nside <- c(7,6)
    }else{
      nside <- c(7,7)
    }
  }else if(num <= 36 && num > 25){
    if(num <= 30){
      nside <- c(6,5)
    }else{
      nside <- c(6,6)
    }
  }else if(num <= 25 && num > 16){
    if(num <= 20){
      nside <- c(5,4)
    }else{
      nside <- c(5,5)
    }
  }else if(num <= 16 && num > 9){
    if(num <= 12){
      nside <- c(4,3)
    }else{
      nside <- c(4,4)
    }
  }else if(num <=  9 && num > 4){
    if(num <= 6){
      nside <- c(3,2)
    }else{
      nside <- c(3,3)
    }
  }else if(num <=  4 && num > 1){
    if(num == 2){
      nside <- c(2,1)
    }else{
      nside <- c(2,2)
    }
  }else{
    nside <- c(1,1)
  }
  return(nside)
}

ile <- function(l, ind, val, replace=FALSE){
  # insert the element 'val' at list 'l' in position given by 'ind'
  # while preserving the rest of the list.
  # i.e. a list of [[1]] 1 [[2]] 2 [[3]] 4
  # with function call(l, 3, 3) will return:
  # [[1]] 1 [[2]] 2 [[3]] 3 [[4]] 4
  # Algorithm: Get the left part of the list, then glue on the 'val'
  #            element and then glue on the right part of the list.
  #            If replace is TRUE, the list element at position
  #            'ind' will be replaced with the 'val'.
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
  if(replace){
    origind <- origind + 1
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

testile <- function(){
  # Test the 'insert list element' (ile) function
  currFuncName <- getCurrFunc()

  l <- list(2,3,4)
  ind <- 1
  val <- 1
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value at beginning of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(9,2,3)
  ind <- 1
  val <- 1
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value at beginning of list with replacement")
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
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value at end of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,9)
  ind <- 3
  val <- 3
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value at end of list with replacement")
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
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value in middle of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,9,3)
  ind <- 2
  val <- 2
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value in middle of list with replacement")
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
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements at beginning of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(9,4,5)
  ind <- 1
  val <- list(1,2,3)
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements at beginning of list with replacement")
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
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements at end of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,9)
  ind <- 3
  val <- list(3,4,5)
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements at end of list with replacement")
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
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements in middle of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,9,5)
  ind <- 2
  val <- list(2,3,4)
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements in middle of list with replacement")
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
