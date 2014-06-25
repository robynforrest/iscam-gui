#**********************************************************************************
# iscam-gui-reptorlist.r
# This file contains the code to load ADMB 'REP' files
#
# Author            : Chris Grandin
# Development Date  : June 2013 - Present
#**********************************************************************************

reptoRlist <- function(fn){
  # Read in the data from the REP file given as 'fn'.
  # File structure:
  # It is assumed that each text label will be on its own line,
  # followed by one or more lines of data.
  # If the label is followed by a single value or line of data,
  #  a vector will be created to hold the data.
  # If the label is followed by multiple lines of data,
  #  a matrix will be created to hold the data. The matrix might be
  #  ragged so a check is done ahead of time to ensure correct
  #  matrix dimensions.
  #
  # If a label has another label following it but no data,
  #  that label is thrown away and not included in the returned list.
  #
  # A label must start with an alphabetic character followed by
  # any number of alphanumeric characters (includes underscore and .)

  dat <- readLines(fn, warn = FALSE)
  # Remove preceeding and trailing whitespace on all elements,
  #  but not 'between' whitespace.
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  # Find the line indices of the labels
  # Labels start with an alphabetic character followed by
  # zero or more alphanumeric characters
  idx  <- grep("^[[:alpha:]]+[[:alnum:]]*", dat)
  objs <- dat[idx]     # A vector of the object names
  nobj <- length(objs) # Number of objects
  ret  <- list()
  indname <- 0

  for(obj in 1:nobj){
    indname <- match(objs[obj], dat)
    if(obj != nobj){ # If this is the last object
      inddata <- match(objs[obj + 1], dat)
    }else{
      inddata <- length(dat) + 1 # Next row
    }
    # 'inddiff' is the difference between the end of data
    # and the start of data for this object. If it is zero,
    # throw away the label as there is no data associated with it.
    inddiff <- inddata - indname
    tmp <- NA
    if(inddiff > 1){
      if(inddiff == 2){
        # Create and populate a vector
        vecdat <- dat[(indname + 1) : (inddata - 1)]
        vecdat <- strsplit(vecdat,"[[:blank:]]+")[[1]]
        vecnum <- as.numeric(vecdat)
        ret[[objs[obj]]] <- vecnum
      }else if(inddiff > 2){
        # Create and populate a (possible ragged) matrix
        matdat <- dat[(indname + 1) : (inddata - 1)]
        matdat <- strsplit(c(matdat), "[[:blank:]]+")
        # Now we have a vector of strings, each representing a row
        # of the matrix, and not all may be the same length
        rowlengths <- unlist(lapply(matdat, "length"))
        nrow <- max(rowlengths)
        ncol <- length(rowlengths)
        # Create a new list with elements padded out by NAs
        matdat <- lapply(matdat, function(.ele){c(.ele, rep(NA, nrow))[1:nrow]})
        matnum <- do.call(rbind, matdat)
        mode(matnum) <- "numeric"
        ret[[objs[obj]]] <- matnum
      }
    }else{
      # Throw away this label since it has no associated data.
    }
  }
  return(ret)
}

read.fit <- function(file){
  # Function to read a basic AD Model Builder fit.
  # Use for instance by:
  #   simple.fit <- read.fit('c:/admb/examples/simple')
  #
  # Then the object 'simple.fit' is a list containing sub
  # 'names', 'est', 'std', 'cor', and 'cov' for all model
  # parameters and sdreport quantities.
  #
  ret <- list()
  parfile <- as.numeric(scan(paste0(file,".par"), what = "", n = 16, quiet = TRUE)[c(6,11,16)])
  ret$nopar   <- as.integer(parfile[1])
  ret$nlogl   <- parfile[2]
  ret$maxgrad <- parfile[3]
  file        <- paste0(file, ".cor")
  lin         <- readLines(file)
  ret$npar    <- length(lin) - 2
  ret$logDetHess <- as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin      <- lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!=''])
  ret$names   <- unlist(lapply(sublin,function(x)x[2]))
  ret$est     <- as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std     <- as.numeric(unlist(lapply(sublin,function(x)x[4])))
  ret$cor     <- matrix(NA, ret$npar, ret$npar)
  corvec      <- unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)]))
  ret$cor[upper.tri(ret$cor, diag=TRUE)] <- as.numeric(corvec)
  ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)]
  ret$cov     <- ret$cor * (ret$std %o% ret$std)
  return(ret)
}

bubble.plot <- function (x=1:dim(z)[1], y=1:dim(z)[2], z, scale = 1, log.scale = FALSE,fill=TRUE, add = FALSE, ...){
  zo <- z
  if (log.scale){
    zo <- log(abs(z) + 1)
  }
  n <- dim(z)[1]
  ny <- dim(z)[2]
  xo <- outer(x, rep(1, length = length(y)))
  yo <- t(outer(y, rep(1, length = length(x))))
  zo <- zo / (max(abs(zo),na.rm = TRUE) * scale) # apply(zo, 2, "/", max(abs(zo))) * length(y) * scale
  zo[abs(zo)<=0.001] <- 0
  nt <- rowSums(z)
  #zo[zo==NA]=0
  if(!add){
    matplot(xo, yo, type = "n", ...)
  }
  for (i in 1:dim(z)[1]) {
    iclr <- rep("transparent", length = ny)
    iclr[z[i, ] <= 0] <- "salmon"
    if(fill){
      print(  zo[i, ])
      points(xo[i, 1:ny], yo[i, 1:ny], cex=abs(zo[i, ]+1e-5), pch=16, col=iclr)
    }
    points(xo[i, 1:ny], yo[i, 1:ny], cex = abs(zo[i, ]+1e-5), pch=1, col="black")
  }
}
