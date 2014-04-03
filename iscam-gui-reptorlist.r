reptoRlist <- function(fn){
  ifile <- scan(fn,what="character",flush=T,blank.lines.skip=F,quiet=T)
  idx <- sapply(as.double(ifile),is.na)
  vnam <- ifile[idx] #list names
  nv <- length(vnam) #number of objects
  A <- list()
  ir <- 0
  for(i in 1:nv){
    ir <- match(vnam[i],ifile)
    if(i!=nv){
      irr <- match(vnam[i+1],ifile)
    }else{
      irr <- length(ifile)+1 #next row
    }
    dum <- NA
    if(irr-ir==2){
      dum <- as.double(scan(fn,skip=ir,nlines=1,quiet=T,what=""))
    }
    if(irr-ir>2){
      dum <- as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=T))
    }
    if(is.numeric(dum)) #Logical test to ensure dealing with numbers
      {
        A[[vnam[i]]] <- dum
      }
  }
  return(A)
}

dattoRlist <- function(data){
  # Parse the object 'data' which has been read in by
  # readData function into its constituent parts

  # Get the element numbers which start with #.
  dat <- grep("^#.*",data)
  # remove the lines that start with #.
  dat <- data[-dat]

  # remove comments which come at the end of a line
  dat <- gsub("#.*","",dat)

  # remove preceeding and trailing whitespace, but not between whitespace
  dat <- gsub("^ +","",dat)
  dat <- gsub(" +$","",dat)

  # Now we have a nice bunch of string elements which are the inputs for iscam.
  # Here we parse them into a list structure
  # This is dependent on the current format of the DAT file and needs to
  # be updated whenever the DAT file changes format
  tmp <- list()
  ind <- 0
  tmp$narea  <- as.numeric(dat[ind <- ind + 1])
  tmp$ngroup <- as.numeric(dat[ind <- ind + 1])
  tmp$nsex   <- as.numeric(dat[ind <- ind + 1])
  tmp$syr    <- as.numeric(dat[ind <- ind + 1])
  tmp$nyr    <- as.numeric(dat[ind <- ind + 1])
  tmp$sage   <- as.numeric(dat[ind <- ind + 1])
  tmp$nage   <- as.numeric(dat[ind <- ind + 1])
  tmp$ngear  <- as.numeric(dat[ind <- ind + 1])
  # Gear allocation
  tmp$alloc  <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  # Age-schedule and population parameters
  tmp$linf   <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmp$k      <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmp$to     <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmp$lwscal <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmp$lwpow  <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmp$age50  <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmp$sd50   <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmp$usemat <- as.numeric(dat[ind <- ind + 1])
  tmp$matvec <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  # Catch data
  tmp$nctobs <- as.numeric(dat[ind <- ind + 1])
  tmp$catch  <- matrix(NA, nrow = tmp$nctobs, ncol = 7)
  for(row in 1:tmp$nctobs){
    tmp$catch[row,] <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  }
  colnames(tmp$catch) <- c("year","gear","area","group","sex","type","value")
  # Abundance indices
  tmp$nit     <- as.numeric(dat[ind <- ind + 1])
  tmp$nitnobs <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmpsurvtype <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  nrows <- sum(tmp$nitnobs)
  tmp$indices <- matrix(NA, nrow = nrows, ncol = 8)
  for(row in 1:nrows){
    tmp$indices[row,] <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  }
  colnames(tmp$indices) <- c("iyr","it","gear","area","group","sex","wt","timing")
  # Age composition data
  tmp$nagears     <- as.numeric(dat[ind <- ind + 1])
  tmp$nagearsvec  <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmp$nagearssage <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmp$nagearsnage <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  tmp$eff         <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
  # This is a ragged object so we must have a list of matrices here,
  # one for each gear (tmp$nagears)
  tmp$agecomps    <- list()
  for(gear in 1:tmp$nagears){
    nrows <- tmp$nagearsvec[gear]
    ncols <- tmp$nagearsnage[gear] - tmp$nagearssage[gear] + 6 # 5 of the 6 here is for the header columns
    tmp$agecomps[[gear]] <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$agecomps[[gear]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
    }
    colnames(tmp$agecomps[[gear]]) <- c("year","gear","area","group","sex",tmp$nagearssage[gear]:tmp$nagearsnage[gear])
  }
  # Empirical weight-at-age data
  tmp$nwttab <- as.numeric(dat[ind <- ind + 1])
  tmp$nwtobs <- as.numeric(dat[ind <- ind + 1])
  tmp$waa <- NULL
  if(tmp$nwtobs > 0){
    # Parse the weight-at-age data
    nrows       <- tmp$nwtobs
    ncols       <- tmp$nage - tmp$sage + 6
    tmp$indices <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$waa[row,] <- as.numeric(strsplit(dat[ind <- ind + 1]," +")[[1]])
    }
    colnames(tmp$indices) <- c("year","gear","area","group","sex",tmp$sage:tmp$nage)
  }
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  return(tmp)
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
  parfile <- as.numeric(scan(paste0(file,".par"),what="", n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar <- as.integer(parfile[1]) 
  ret$nlogl <- parfile[2] 
  ret$maxgrad <- parfile[3] 
  file <- paste(file,".cor", sep="") 
  lin <- readLines(file) 
  ret$npar <- length(lin)-2 
  ret$logDetHess <- as.numeric(strsplit(lin[1], '=')[[1]][2]) 
  sublin <- lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!='']) 
  ret$names <- unlist(lapply(sublin,function(x)x[2])) 
  ret$est <- as.numeric(unlist(lapply(sublin,function(x)x[3]))) 
  ret$std <- as.numeric(unlist(lapply(sublin,function(x)x[4]))) 
  ret$cor <- matrix(NA, ret$npar, ret$npar) 
  corvec <- unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)])) 
  ret$cor[upper.tri(ret$cor, diag=TRUE)] <- as.numeric(corvec) 
  ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)] 
  ret$cov <- ret$cor*(ret$std%o%ret$std) 
  return(ret) 
} 

bubble.plot <- function (x=1:dim(z)[1], y=1:dim(z)[2], z, scale = 1, log.scale = FALSE,fill=T,add=F, ...){
  zo <- z
  if (log.scale){
    zo <- log(abs(z) + 1)
  }
  n <- dim(z)[1]
  ny <- dim(z)[2]
  xo <- outer(x, rep(1, length = length(y)))
  yo <- t(outer(y, rep(1, length = length(x))))
  zo <- zo/(max(abs(zo),na.rm=T) *scale) #apply(zo, 2, "/", max(abs(zo))) * length(y) * scale
  zo[abs(zo)<=0.001] <- NA
  nt <- rowSums(z)
  #zo[zo==NA]=0
  if(!add){
    matplot(xo, yo, type = "n", ...)
  }
  for (i in 1:dim(z)[1]) {
    iclr <- rep("transparent", length = ny)
    iclr[z[i, ] <= 0] <- "salmon"
    if(fill){
      points(xo[i, 1:ny], yo[i, 1:ny], cex=abs(zo[i, ]), pch=16, col=iclr)
    }
    points(xo[i, 1:ny], yo[i, 1:ny], cex = abs(zo[i, ]), pch=1, col="black")
  }   
}
