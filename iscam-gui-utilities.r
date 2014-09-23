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
  quants <- apply(data, 2, quantile, probs)
  return(quants)
}

drawEnvelope <- function(yrs, quants, color, yUpper, first, ...){
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
    plot(yrs, median, type="l", col=color, lty=1, lwd=2, ylim=c(0,yUpper), ...)
  }else{
    lines(yrs, median, type="l", col=color, lty=1, lwd=2, ylim=c(0,yUpper), ...)
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
    numRetros <- length(op[[models[1]]]$outputs$retros) + 1  # +1 for the base
    hasType <- vector("numeric", length = numRetros)
    for(model in 1:numRetros){
      if(model == 1){
        # base model
        hasType[[model]] <- !is.null(unlist(op[[models[model]]]$outputs[type]))
      }else{
        hasType[[model]] <- !is.null(unlist(op[[models[1]]]$outputs$retros[[model-1]]$outputs[type]))
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
  inputs <- out <- colors <- linetypes <- names <- vector("list", len <- sum(hasType))
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
        out[[model]] <- op[[models[1]]]$outputs[type]
        names[[model]]  <- op[[models[1]]]$names$scenario
        inputs[[model]] <- op[[models[1]]]$inputs$data
        linetypes[[model]] <- op[[models[1]]]$inputs$linetype
      }else{
        out[[model]]    <- op[[models[1]]]$outputs$retros[[model-1]]$outputs[type]
        names[[model]]  <- op[[models[1]]]$outputs$retros[[model-1]]$names$scenario
        inputs[[model]] <- op[[models[1]]]$outputs$retros[[model-1]]$inputs$data
        linetypes[[model]] <- op[[models[1]]]$outputs$retros[[model-1]]$inputs$linetype
      }
      colors[[model]] <- .RETRO_COLORS[model]
    }else{
      out[[model]]    <- op[[models[model]]]$outputs[type]
      colors[[model]] <- op[[models[model]]]$inputs$color
      names[[model]]  <- op[[models[model]]]$names$scenario
      inputs[[model]] <- op[[models[model]]]$inputs$data
      linetypes[[model]] <- op[[models[model]]]$inputs$linetype
    }
  }
  if(length(out) == 1 && is.null(out[[1]][[1]])){
    return(NULL)
  }
  ret <- list(out,colors,names,inputs,linetypes)
  return(ret)
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  # Return TRUE if x is an integer, FALSE otherwise
  abs(x - round(x)) < tol
}
