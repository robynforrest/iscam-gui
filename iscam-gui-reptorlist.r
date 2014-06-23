#**********************************************************************************
# iscam-gui-reptorlist.r
# This file contains the code to load ADMB 'REP' files
#
# Author            : Chris Grandin, and many others
# Development Date  : June 2014 - Present
#**********************************************************************************

reptoRlist <- function(fn){
  # Function to read an ADMB 'REP' (report) file..
  ifile <- scan(fn, what = "character", flush = TRUE, blank.lines.skip = FALSE, quiet = TRUE)
  idx   <- sapply(as.double(ifile), is.na)
  vnam  <- ifile[idx]   # list names
  nv    <- length(vnam) # number of objects
  A     <- list()
  ir    <- 0
  for(i in 1:nv){
    ir <- match(vnam[i], ifile)
    if(i != nv){
      irr <- match(vnam[i + 1], ifile)
    }else{
      irr <- length(ifile) + 1 #next row
    }
    dum <- NA
    if(irr - ir == 2){
      dum <- as.double(scan(fn, skip = ir, nlines = 1, quiet = TRUE, what = ""))
    }
    if(irr-ir > 2){
      # Read matrix objects
      # Extra lines are for ragged arrays where first row is shorter than subsequent rows
      count <- 0
      nCol <- 1
      #find the longest row in the matrix
      for(ii in ir:(irr-2)){
        tmp <- as.double(scan(fn, skip = ir + count, nlines = 1, quiet = TRUE, what = ""))
        ltmp<- length(tmp)
        if(ltmp > nCol) nCol <- ltmp # get length of longest row
        count <- count+1
      }

      count<- 0
      dum <- matrix(ncol = nCol, nrow = irr - ir - 1)

      for(ii in 1:length(ir:(irr-2))) {
        tmp <- as.double(scan(fn, skip = ir + count, nlines = 1, quiet = TRUE, what = ""))
        ltmp <- length(tmp)
        if(ltmp < nCol) {
          tmp[(ltmp+1):nCol] <- NA
        }
        # Fill the matrix
        dum[ii,] <- tmp
        count <- count + 1
      }
     }
     # Old way, assumes non-ragged matrix
     # dum <- as.matrix(read.table(fn,skip=ir, nrow=irr-ir-1,fill = TRUE))

    if(is.numeric(dum)){ #Logical test to ensure dealing with numbers
      A[[vnam[i]]] <- dum
    }
  }
  return(A)
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
