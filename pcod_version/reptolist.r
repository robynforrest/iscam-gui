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
