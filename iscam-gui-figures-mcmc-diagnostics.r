#**********************************************************************************
# iscam-gui-figures-mcmc-convergence.r
# This file contains the code to plot mcmc convergence information
#
# Author            : Chris Grandin
# Development Date  : April 2014 - Present
#**********************************************************************************

plotConvergence <- function(scenario   = 1,         # Scenario number
                            plotNum    = 1,         # Plot code number
                            savefig    = .SAVEFIG,  # TRUE/FALSE for plot output
                            fileText   = "Default", # Name of the file if png==TRUE
                            nchains    = 2,         # Number of chains to use in the gelman diagnostics
                            # PlotSpecs: Width, height, and resolution of screen and file
                            ps         = list(pngres = .RESOLUTION,
                                              pngw   = .WIDTH,
                                              pngh   = .HEIGHT,
                                              res    = .RESOLUTION,
                                              w      = .WIDTH,
                                              h      = .HEIGHT),
                            figtype    = .FIGURE_TYPE, # The filetype of the figure with period, e.g. ".png"
                            showtitle  = TRUE,         # Show the main title on the plot
                            burnthin   = list(0,1), # List of two elements, burnin and thinning
                            exFactor        = 1.5,
                            showEntirePrior = TRUE,
                            units           = .UNITS,
                            silent          = .SILENT,
                            latexnames      = FALSE,  # If TRUE, latex-pretty parameter names will be inserted
                            priorsonly      = FALSE){ # Plot priors only in the priors vs posts function

  # Plot a convergence plot for an MCMC model run
  # plotNum must be one of:
  # 1  Trace plots
  # 2  Autocorrelation plots
  # 3  Density plots
  # 4  Pairs plots with histograms
  # 5  Priors vs. Posteriors plots
  # 6  Variance partitions
  # 7  Gelman Rubin Statistic plot, to show convergence of mcmc.
  #    mcmc chain will be broken into nchains individual chains by the routine.

  currFuncName <- getCurrFunc()
  scenarioName <- op[[scenario]]$names$scenario
  mcmcOut      <- op[[scenario]]$outputs$mcmc
  mpdData      <- op[[scenario]]$outputs$mpd #For MLE estimates (Prior/Post plot)
  inputs       <- op[[scenario]]$inputs # For the priors information
  
  if(is.null(mcmcOut)){
    cat0(.PROJECT_NAME,"->",currFuncName,"The model ",scenarioName," has no mcmc output data associated with it.\n")
    return(NULL)
  }

  figDir       <- op[[scenario]]$names$figDir
  res          <- ps$pngres
  width        <- ps$pngw
  height       <- ps$pngh
  resScreen    <- ps$res
  widthScreen  <- ps$w
  heightScreen <- ps$h

  if(priorsonly){
    filenameRaw <- paste0(scenarioName,"_priorsOnly",figtype)
  }else{
    filenameRaw  <- paste0(scenarioName,"_",fileText,figtype)
  }
  filename     <- file.path(figDir,filenameRaw)
  if(savefig){
    graphics.off()
    if(figtype == .PNG_TYPE){
      png(filename,res=res,width=width,height=height,units=units)
    }
    if(figtype == .EPS_TYPE){
      setEPS(horizontal=FALSE, onefile=FALSE, paper="special",width=width,height=height)
      postscript(filename)
    }
  }else{
    ##if(dev.cur() != 1){
    ##  dev.off()
    ## }
    ##windows(width=widthScreen,height=heightScreen)
  }

  mcmcData <- mcmcOut$params

  # If you are trying to show more than one group or area's parameters,
  # comment the next line below out.
  mcmcData <- stripAreasGroups(mcmcData)
  # We only want to see estimated parameters, so this call strips the static params.
  mcmcData <- stripStaticParams(scenario, mcmcData)
  
  if(plotNum == 1){
    plotTraces(mcmcData, burnthin=burnthin, showtitle = showtitle, latexnames=latexnames)
  }
  if(plotNum == 2){
    plotAutocor(mcmcData, burnthin=burnthin, showtitle = showtitle, latexnames=latexnames)
  }
  if(plotNum == 3){
    plotDensity(mcmcData, burnthin=burnthin, showtitle = showtitle, latexnames=latexnames)
  }
  if(plotNum == 4){
    plotPairs(mcmcData, burnthin=burnthin, showtitle = showtitle, latexnames=latexnames)
  }
  if(plotNum == 5){
    plotPriorsPosts(mcmcData, mpdData, inputs = inputs, burnthin=burnthin, showtitle = showtitle, priorsonly = priorsonly, latexnames=latexnames)
  }
  if(plotNum == 6){
    plotVariancePartitions(mcmcData, burnthin=burnthin, showtitle = showtitle, latexnames=latexnames)
  }
  if(plotNum == 7){
    plotGelman(mcmcData, nchains=nchains, burnthin=burnthin, showtitle = showtitle, latexnames=latexnames)
    #plotCumulativePosterior(mcmcData, burnthin=burnthin, showtitle = showtitle, latexnames=latexnames)
    #plotCumulativePosterior_Rowan(mcmcData, burnthin=burnthin, showtitle = showtitle, latexnames=latexnames, col.trace=c("green","red","blue"))
  }
  if(savefig){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

stripAreasGroups <- function(dat){
  # This is a hack function to remove the area and group prefixes for the mcmc data 'dat'
  # The reason is that for now we are just working with a single group and area, and
  # the extra text in the parameter names are confusing, eg. 'ro_gr1' will become just 'ro'
  # If you make a model with more than one group or area this will need to be revisited.
  # ALSO this removes 'f' which is assumed to be the objective function value
  # Note than q1, q2, q3... will stay the same and m1 and m2 will remain if the model was two-sex.

  pnames <- names(dat)
  # M will only ever be 1 or 2, for each sex
  pnames <- gsub("m_gs1","m1",pnames)
  pnames <- gsub("m_gs2","m2",pnames)

  pnames <- gsub("msy1","msy",pnames)
  pnames <- gsub("fmsy1","fmsy",pnames)
  pnames <- gsub("SSB1","ssb",pnames)
  pnames <- gsub("sel_sd([0-9]+)","selsd\\1",pnames)
  pnames <- gsub("sel_g([0-9]+)","sel\\1",pnames)
 
  # Remove underscores
  names(dat) <- gsub("_+.*","",pnames)
  # Remove objective function value
  dat <- dat[,names(dat) != "f"]
  return(dat)
}

stripStaticParams <- function(scenario, dat){
  # Strip out the static (non-estimated) parameters from the mcmc output data
  # for the given scenario.
  # We only need to see estimated parameters on the diagnostic plots!
  # If there are no static parameters, NULL will be returned

  # Check the control file to see which parameters were static
  inp <- as.data.frame(op[[scenario]]$inputs$control$param)
  
  static <- inp[inp$phz <= 0,]
  snames <- rownames(static)

  # Now remove those from the mcmc data
  pnames <- names(dat)
  # remove the log_ stuff from the input parameter names
  snames <- gsub("log_","",snames)

  # There will be either one "m" or "m1" and "m2" in pnames.
  # If "m" is in snames, remove the "m1", and "m2" from pnames as well if they exist
  if("m" %in% snames){
    ms <- c("m1","m2")
    snames <- c(snames, "m1", "m2")
  }

    # The following also removes "m" in a combined sex model
    dat <- dat[,!(pnames %in% snames)]
 
  # Remove static selectivity params
  selParams <- as.data.frame(op[[scenario]]$inputs$control$sel)
  estphase <- selParams["estphase",]
  staticSel <- estphase<1
  selPostNames <- names(dat)[grep("sel[0-9]+",names(dat))]
  selPostNames <- selPostNames[staticSel]
  selSDPostNames <- names(dat)[grep("selsd[0-9]+",names(dat))]
  selSDPostNames <- selSDPostNames[staticSel]
  datNames <- names(dat)
  staticSelInds <- NULL
  if(length(selPostNames) > 0){
    # If there are static parameters, remove them.
    for(staticSel in 1:length(selPostNames)){
      staticSelInds <- c(staticSelInds, grep(selPostNames[staticSel], datNames))
      staticSelInds <- c(staticSelInds, grep(selSDPostNames[staticSel], datNames))
    }
    dat <- dat[,-staticSelInds]
  }

  datNames <- names(dat)
  return(dat)
}

plotCumulativePosterior_Rowan<- function(mcmcData = NULL, burnthin = list(0,1),
                                    nchains=3, axes=FALSE, same.limits=FALSE, between=list(x=axes,y=axes),
                                    div=1, span=1/4, log=FALSE, base=10, main=NULL, xlab=NULL, ylab=NULL,
                                    cex.main=1.2, cex.lab=1, cex.strip=0.8, cex.axis=0.8, las=0,
                                    tck=0.5, tick.number=5, lty.trace=1, lwd.trace=1, col.trace="grey",
                                    lty.median=1, lwd.median=1, col.median="black", lty.quant=2, lwd.quant=1,
                                    col.quant="black", plot=TRUE, probs=c(0.025, 0.5, 0.975),
                                    showtitle = TRUE, latexnames=latexnames, ...){
  # Cumulative values of posterior
  # Break burned-in chain into 'nchains' segments, and create a three-panel cumulative plot of each of the segments

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burnin       <- burnthin[[1]]
  thinning     <- burnthin[[2]]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  panel.trace <- function(x, y, ...) {
		panel.xyplot(x, y, type="n")
		chainlink=rep(1:nchains,f)
		for (i in 1:nchains) {
			z=is.element(chainlink,i)
			panel.xyplot(x[z], y[z], type="l", lty=lty.trace, lwd=2, col=rep(col.trace,nchains)[i])
		}
		#panel.xyplot(x, y, type="l", lty=lty.trace, lwd=lwd.trace, col=col.trace)
	}

  mcmcData <- window(as.matrix(mcmcData), start=burnin, thin=thinning)
  mcmc <- as.data.frame(mcmcData)

  n <- nrow(mcmc)
	f <- rep(round(n/nchains),nchains-1); f=c(f,n-sum(f))
	p <- ncol(mcmc)
	dat <- data.frame(Factor=ordered(rep(names(mcmc), each=n), names(mcmc)),
                    Draw=rep(1:n, p),
                    Chain=rep(rep(1:nchains,f),p),
                    Value=as.vector(as.matrix(mcmc)))
	mess <- c("require(grid, quietly=TRUE, warn.conflicts=FALSE)",
            "require(lattice, quietly=TRUE, warn.conflicts=FALSE)")
	eval(parse(text=mess))
	if (trellis.par.get()$background$col == "#909090") {
		for (d in dev.list()) dev.off()
		trellis.device(color=FALSE)
	}

	relation <- if (same.limits) "same" else "free"
  mymain <- list(label="cumu", cex=1)
	myxlab <- list(label="X", cex=1)
	myylab <- list(label="Y", cex=1)
	myrot <- switch(as.character(las), `0`=0, `1`=0, `2`=0, `3`=90)     # AME changed '0'=90 to 0
	myscales <- list(x=list(draw=FALSE, relation=relation, cex=1, tck=0.5, tick.number=5, rot=myrot),
                   y=list(draw=FALSE, relation=relation, cex=1, tck=0.5, tick.number=5, rot=myrot))
	mystrip <- list(cex=1)

  dat$Index <- paste(dat$Factor,dat$Chain,sep="-")
	vList <- split(dat$Value,dat$Index)
	qList <- sapply(vList,function(x){xsort=sort(x)
                                    xscal=xsort - min(xsort)
                                    ycumu=cumsum(xscal)/sum(xscal)
                                    out=cbind(x=xsort,y=ycumu)
                                    return(out)},
                  simplify=FALSE)
	dat$CumFreq <- dat$ValueSort <- NA
	for (i in names(qList)) {
		z <- is.element(dat$Index,i)
		dat$ValueSort[z] <- qList[[i]][,"x"]
		dat$CumFreq[z]   <- qList[[i]][,"y"]
	}
  if(latexnames){
    #unames <- unique(dat$Factor)
#   for(name in 1:nrow(dat)){
#     dat$Factor[name] <- getLatexName(dat$Factor[name])
#   }
  }
	graph <- xyplot(CumFreq ~ ValueSort  | Factor, panel=panel.trace,
                  data=dat, as.table=TRUE, between=between, main=mymain,
                  xlab=myxlab, ylab=myylab, par.strip.text=mystrip,
                  scales=myscales, ylim=c(0,1), ...)
  print(graph)
}

plotCumulativePosterior <- function(mcmcData = NULL, burnthin = list(0,1), nsegs=3, color.seg=c("red","green","blue"), showtitle = TRUE, latexnames=latexnames){
  # Cumulative values of posterior for mcmcData matrix
  # Break burned-in chain into 'nchains' segments, and create a three-panel cumulative plot of each of the segments
  # color.seg must be the length determined by nsegs

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burnin       <- burnthin[[1]]
  thinning     <- burnthin[[2]]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  if(length(color.seg) != nsegs){
    cat0(.PROJECT_NAME,"->",currFuncName,"'color.seg' vector is not of length 'nsegs'.\n")
    return(NULL)
  }
  # np is number of parameters
  np <- ncol(mcmcData)
  nr <- nrow(mcmcData)
  nside <- getRowsCols(np)
  par(mfrow = nside, oma = c(2,3,1,1), mai = c(0.2,0.4,0.3,0.2))

  mcmcData <- window(as.matrix(mcmcData), start=burnin, thin=thinning)
  # Number of rows in each segment of the chain. If there are residual records
  # due to the floor command, they will be truncated from the end of the overall chain
  numperseg <- floor(nr/nsegs)
  for(param in 1:np){
    par(mar=.MCMC_MARGINS)
    mcmcTrace <- as.matrix(mcmcData[,param])
    name <- colnames(mcmcData)[param]
    if(latexnames){
      name <- getLatexName(name)
    }
    start <- 1
    for(seg in 1:nsegs){
      dat <- mcmcTrace[start:(start+numperseg-1)]
      # Calculate cumulative object for this chain segment
      #sdat <- sort(dat)
      #scaldat <- sdat - min(sdat)
      #cumdat <- cumsum(scaldat) / sum(scaldat)
      cumdat <- cumsum(dat) / sum(dat)
      if(start==1){
        plot(1:length(cumdat), cumdat, main=name, type="l", ylab="", xlab="", col=color.seg[seg], axes=FALSE)
      }else{
        lines(1:length(cumdat), cumdat, col=color.seg[seg], axes=FALSE)
      }
      start <- start + numperseg
     }
    box()
    #at <- labels <- seq(0,nrow(mcmcData), axis.lab.freq)
    #axis(1, at=at, labels=labels)
    #axis(2)
  }
}

plotTraces <- function(mcmcData = NULL, burnthin = list(0,1), axis.lab.freq=200, showtitle = TRUE, latexnames=latexnames){
  # Traceplots for an mcmc matrix, mcmcData
  # axis.lab.freq is the frequency of x-axis labelling

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burnin       <- burnthin[[1]]
  thinning     <- burnthin[[2]]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  # np is number of parameters
  np <- ncol(mcmcData)
  nside <- getRowsCols(np)
  par(mfrow = nside, oma = c(2,3,1,1), mai = c(0.2,0.4,0.3,0.2))

  mcmcData <- window(as.matrix(mcmcData), start=burnin, thin=thinning)
  for(param in 1:np){
    par(mar=.MCMC_MARGINS)
    mcmcTrace <- as.matrix(mcmcData[,param])
    name <- colnames(mcmcData)[param]
    if(latexnames){
      name <- getLatexName(name)
    }
    plot(mcmcTrace, main=name, type="l", ylab="", xlab="", axes=FALSE)
    box()
    at <- labels <- seq(0,nrow(mcmcData), axis.lab.freq)
    axis(1, at=at, labels=labels)
    axis(2)
  }
}

getLatexName <- function(name, addToQ = 0){
  # Return a pretty version of the parameter name found in variable 'name'
  # addToQ is an integer to the parameter name for the q's. This is necessary
  # because iscam sets the q parameter names to 1, 2, 3... regardless of the
  # gear number. i.e. if gear 1 is a trawl fishery and gear 2 is a survey,
  # iscam will call q1 the survey gear. We must add 1 to it to get q2 to
  # accurately portray the survey gear number
  if(name == "ro") return(expression("R"[0]))
  if(name == "rbar") return(expression(bar("R")))
  if(name == "rinit") return(expression(bar("R")[init]))
  if(name == "bo") return(expression("B"[0]))
  if(name == "vartheta") return(expression(vartheta))
  if(name == "rho") return(expression(rho))
  if(name == "bmsy") return(expression("B"[MSY]))
  if(name == "msy") return(expression("MSY"))
  if(name == "fmsy") return(expression("F"[MSY]))
  if(name == "umsy") return(expression("U"[MSY]))
  if(name == "ssb") return(expression("SSB"))
  if(name == "sel1") return(expression(hat(a)[1]))
  if(name == "selsd1") return(expression(hat(gamma)[1]))
  if(name == "sel2") return(expression(hat(a)[2]))
  if(name == "selsd2") return(expression(hat(gamma)[2]))
  if(name == "sel3") return(expression(hat(a)[3]))
  if(name == "selsd3") return(expression(hat(gamma)[3]))
  if(name == "sel4") return(expression(hat(a)[4]))
  if(name == "selsd4") return(expression(hat(gamma)[4]))
  if(name == "sel5") return(expression(hat(a)[5]))
  if(name == "selsd5") return(expression(hat(gamma)[5]))

  if(name == "log_ro") return(expression("ln(R"[0]*")"))
  if(name == "h") return(expression("h"))
  if(name == "m1") return(expression("M"[1]))
  if(name == "m2") return(expression("M"[2]))
  if(name == "log_m") return(expression("ln(M)"))
  if(name == "log_rbar") return(expression("ln("*bar("R")*")"))
  if(name == "log_rinit") return(expression("ln("*bar("R")[init]*")"))

  if(name == "q1"){
    digit <- 1 + addToQ
    return(substitute("q"[digit], list(digit=digit)))
  }
  if(name == "q2"){
    digit <- 2 + addToQ
    return(substitute("q"[digit], list(digit=digit)))
  }
  if(name == "q3"){
    digit <- 3 + addToQ
    return(substitute("q"[digit], list(digit=digit)))
  }
  if(name == "q4"){
    digit <- 4 + addToQ
    return(substitute("q"[digit], list(digit=digit)))
  }
  if(name == "q5"){
    digit <- 5 + addToQ
    return(substitute("q"[digit], list(digit=digit)))
  }
  if(name == "q6"){
    digit <- 6 + addToQ
    return(substitute("q"[digit], list(digit=digit)))
  }
  if(name == "q7"){
    digit <- 7 + addToQ
    return(substitute("q"[digit], list(digit=digit)))
  }
  if(name == "q8"){
    digit <- 8 + addToQ
    return(substitute("q"[digit], list(digit=digit)))
  }

  if(name == "log_q1"){
    digit <- 1 + addToQ
    return(substitute("ln(q"[digit]*")", list(digit=digit)))
  }
  if(name == "log_q2"){
    digit <- 2 + addToQ
    return(substitute("ln(q"[digit]*")", list(digit=digit)))
  }
  if(name == "log_q3"){
    digit <- 3 + addToQ
    return(substitute("ln(q"[digit]*")", list(digit=digit)))
  }
  if(name == "log_q4"){
    digit <- 4 + addToQ
    return(substitute("ln(q"[digit]*")", list(digit=digit)))
  }
  if(name == "log_q5"){
    digit <- 5 + addToQ
    return(substitute("ln(q"[digit]*")", list(digit=digit)))
  }
  if(name == "log_q6"){
    digit <- 6 + addToQ
    return(substitute("ln(q"[digit]*")", list(digit=digit)))
  }
  if(name == "log_q7"){
    digit <- 7 + addToQ
    return(substitute("ln(q"[digit]*")", list(digit=digit)))
  }
  if(name == "log_q8"){
    digit <- 8 + addToQ
    return(substitute("ln(q"[digit]*")", list(digit=digit)))
  }

  return(NULL)
}

plotAutocor <- function(mcmcData = NULL,
                        burnthin = list(0,1),
                        lag = c(0, 1, 5, 10, 15, 20, 30, 40, 50),
                        latexnames = FALSE,
                        showtitle = TRUE){
  # Plot autocorrelations for all mcmc parameters
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burnin       <- burnthin[[1]]
  thinning     <- burnthin[[2]]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  np <- ncol(mcmcData)
  nside <- getRowsCols(np)
  par(mfrow = nside, oma = c(2,3,1,1), mai = c(0.2,0.4,0.3,0.2))

  for(param in 1:np){
    par(mar=.MCMC_MARGINS)
    mcmcAutocor <- window(mcmc(as.ts(mcmcData[,param])), start = burnin, thin = thinning)
    name <- colnames(mcmcData)[param]
    if(latexnames){
      name <- getLatexName(name)
    }
    autocorr.plot(mcmcAutocor, lag.max = 100, main = name, auto.layout = FALSE)
  }
}

plotDensity <- function(mcmcData = NULL, burnthin = list(0,1), color = 1, opacity = 30, showtitle = TRUE, latexnames=FALSE){
  # Plot densities for the mcmc parameters in the matrix mcmcData
	oldPar	<- par(no.readonly=T)
  on.exit(par(oldPar))

  burnin       <- burnthin[[1]]
  thinning     <- burnthin[[2]]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  np <- ncol(mcmcData)
  nside <- getRowsCols(np)
  par(mfrow = nside, oma = c(2,3,1,1), mai = c(0.2,0.4,0.3,0.2))

  for(param in 1:np){
    par(mar=.MCMC_MARGINS)
    dat <- window(mcmc(as.ts(mcmcData[,param])), start = burnin, thin = thinning)
    dens <- density(dat)
    name <- colnames(mcmcData)[param]
    if(latexnames){
      name <- getLatexName(name)
    }
    plot(dens, main = name, ylab="")
    xx <- c(dens$x,rev(dens$x))
    yy <- c(rep(min(dens$y), length(dens$y)), rev(dens$y))
    shade <- .getShade(color, opacity)
    polygon(xx, yy, density = NA, col = shade)
  }
}

plotPairs <- function(mcmcData = NULL, burnthin = list(0,1), showtitle = TRUE, latexnames=FALSE){
  # Pairs plots for an mcmc matrix, mcmcData

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burnin       <- burnthin[[1]]
  thinning     <- burnthin[[2]]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }

  panel.hist <- function(x, ...){
    usr    <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h      <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y      <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="wheat", cex=0.75, ...)
  }

  # Remove the reference point posteriors for this plot
  datNames <- names(mcmcData)
  refptInd <- c(grep("bo",datNames),grep("bmsy",datNames),grep("fmsy",datNames),grep("msy",datNames),grep("ssb",datNames))
  mcmcData <- mcmcData[,-refptInd]

  numParams <- ncol(mcmcData)
  mcmcData <- window(as.matrix(mcmcData), start=burnin, thin=thinning)
  names <- colnames(mcmcData)
  if(latexnames){
    newnames <- NULL
    for(col in 1:ncol(mcmcData)){
      newnames <- c(newnames, getLatexName(names[col]))
    }
    #colnames(mcmcData) <- newcoln
    names <- newnames
  }
  pairs(mcmcData, labels=names, cex.labels=1.0, pch=".", upper.panel = panel.smooth, diag.panel = panel.hist, lower.panel = panel.smooth, gap=0.0)
}

plotPriorsPosts <- function(mcmcData, mpdData, inputs = NULL, burnthin = list(0,1), color = 1, opacity = 30, showtitle = TRUE, priorsonly=FALSE, latexnames=FALSE){
  # Produce a grid of the parameters' posteriors with their priors overlaid.
  # mpdData is used to get the MLE estimates for each parameter
	oldPar	<- par(no.readonly=T)
  on.exit(par(oldPar))

  burnin       <- burnthin[[1]]
  thinning     <- burnthin[[2]]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(is.null(inputs)){
    cat0(.PROJECT_NAME,"->",currFuncName,"inputs must be supplied so that the priors can be plotted over the posteriors.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  # The values in the control file (inputs$control$param) for each of priorN are:
  # 1. ival  = initial value
  # 2. lb    = lower bound
  # 3. ub    = upper bound
  # 4. phz   = ADMB phase
  # 5. prior = prior distribution funnction
  #             0 = Uniform
  #             1 = normal    (p1=mu,p2=sig)
  #             2 = lognormal (p1=log(mu),p2=sig)
  #             3 = beta      (p1=alpha,p2=beta)
  #             4 = gamma     (p1=alpha,p2=beta)
  # 6. p1 (defined by 5 above)
  # 7. p2 (defined by 5 above)
  fNames <- c(dunif,dnorm,dlnorm,dbeta,dgamma)

  numParams <- ncol(mcmcData)
  qParams <- inputs$control$survq
  numQParams <- ncol(qParams)
  priorSpecs <- as.data.frame(inputs$control$param)

  # Remove fixed parameters
  priorSpecs <- priorSpecs[priorSpecs$phz>0,]
  # Remove upper and lower bound, and phase information, but keep initial value
  priorSpecs <- priorSpecs[,-c(2:4)]

  # Add in the q parameters to the prior specs table
  for(q in 1:numQParams){
    # Add a row for each q to the paramSpecs matrix
    priorSpecs <- rbind(priorSpecs, c(qParams[2,q], qParams[1,q], qParams[2,q], qParams[3,q]))
    rownames(priorSpecs)[nrow(priorSpecs)] <- paste0("log_q",q)
  }
  priorNames <- rownames(priorSpecs)
  postNames <- names(mcmcData)

  # Add the selectivity parameters to the prior specs table here
  # inputs$control$sel

  if(length(grep("^m[12]$",postNames)) == 2){
    # Remove the single 'm' and add the two m's, log_m1 and log_m2 to the prior paramSpecs table
    # because the posterior output has two m's
    mSpecInd <- grep("m",priorNames)
    mSpec <- priorSpecs[mSpecInd,]
    priorSpecs <- priorSpecs[-mSpecInd,]
    # Add each m1 and m2 in
    mSpec1 <- mSpec2 <- mSpec
    rownames(mSpec1) <- "log_m1"
    rownames(mSpec2) <- "log_m2"
    priorSpecs <- rbind(mSpec1,mSpec2,priorSpecs)
  }else{
    # Only one m, so make it log_m
#    mSpecInd <- grep("m",priorNames)
#    mSpec <- priorSpecs[mSpecInd,]
#    priorSpecs <- priorSpecs[-mSpecInd,]
    # Add each m1 and m2 in
#    rownames(mSpec) <- "log_m"
#    priorSpecs <- rbind(mSpec,priorSpecs)
  }
  priorNames <- rownames(priorSpecs)
  numWithPriors <- length(priorNames)
  nside <- getRowsCols(numWithPriors)
  par(mfrow = nside, oma = c(2,3,1,1), mai = c(0.2,0.4,0.3,0.2))


  for(postInd in 1:ncol(mcmcData)){
    # Find the parameter name from mcmcData in the priorSpecs table
    # and plot if it is in the paramSpecs table
    # Make sure that the name matches exactly by adding anchors ^ and $ to the pattern
    # and log_ is optional
    postName <- names(mcmcData)[postInd]
    priorPattern <- paste0("^[log_]*",postName,"$")
    if(postName == "m1"){
      priorPattern <- "log_m"
    }
    priorInd <- grep(priorPattern, priorNames)
    if(length(priorInd) > 0){
      # The posterior name is in the list of priors..
      dat <- mcmcData[,postInd]
      if(postName != "h"){  # HACK!!! iScam should output its results in the same space as input parameters
        dat <- log(dat)
      }
      dat <- window(mcmc(as.ts(dat), start = burnin, thin = thinning))

      specs <- unlist(priorSpecs[priorInd,])
      priorfn <- fNames[[specs[2]+1]] # +1 because the control prior starts at zero

      # Get matching MLE estimate for this parameter from mpdData
      pName <- priorNames[priorInd]
      qpat <- "log_q([1-9])+$"
      if(pName == "log_m1"){
        mle <- mpdData$m[1]
      }else if(pName == "log_m2"){
        mle <- mpdData$m[2]
      }else if(pName == "log_m"){
        mle <- mpdData$m[2]
      }else if(pName == "h"){
        mle <- mpdData$steepness
      }else if(length(grep(qpat,pName)) > 0){
        num <- as.numeric(sub(qpat, "\\1", pName))
        mle <- mpdData$q[num]
      }else{
        mle <- mpdData[postName]
      }
      xx <- list(p = dat, p1 = specs[3], p2 = specs[4], fn = priorfn, nm = priorNames[priorInd], mle=mle)
      par(mar=.MCMC_MARGINS)
      if(latexnames){
        xx$nm <- getLatexName(xx$nm)
      }
      if(priorsonly){
        func <- function(x){xx$fn(x,xx$p1,xx$p2)}
        if(specs[2] == 0){
          # Uniform, plot from p1-1 to p2+1
          curve(func, from = xx$p1 - 1, to = xx$p2 + 1, xlab="", ylab="", col="green", lwd=2)
        }else if(specs[2] == 1){
          # Normal, plot from -(p1-p2*4) to (p1+p2*4)
          curve(func, from = xx$p1 - 4 * xx$p2, to = xx$p2 + 4 * xx$p2, xlab="", ylab="", col="green", lwd=2)
        }else{
          curve(func, xlab="", ylab="", col="green", lwd=2)
        }
        if(!priorsonly){
          abline(v = xx$mle, lwd=2, lty=2, col=2)
        }
        title(xx$nm)
      }else{
        plot.marg(xx, breaks = "sturges", col = "wheat")
      }
    }
  }
}

plot.marg <- function(xx, breaks = "sturges", exFactor = 1.0, ...){
  # xx is a list(p=samples, p1=prior param 1, p2=prior param 2, fn=prior distribution)
  #  and ignore posterior distribution limits
  posteriorNoPlot <- hist(xx$p, breaks = breaks, plot=FALSE)
  xvals <- seq(min(posteriorNoPlot$breaks)/exFactor, max(posteriorNoPlot$breaks)/exFactor, length=1000)
  pd <- xx$fn(xvals, xx$p1, xx$p2)
  z <- cbind(xvals, pd)

  xlim <- c(min(xvals), max(xvals))
  ss <- hist(xx$p, prob=TRUE, breaks = breaks, main = xx$nm, xlab="", cex.axis = 1.2, xlim = xlim, ylab = "", ...)
  #ss <- barplot(xx$p, prob=TRUE, breaks = breaks, main = xx$nm, xlab="", cex.axis = 1.2, xlim = xlim, ylab = "", ...)
  func <- function(x){xx$fn(x,xx$p1,xx$p2)}
  curve(func, xlim[1], xlim[2], xlab="", ylab="", col="green", lwd=2, add=TRUE) ##, axis=FALSE)
  abline(v = xx$mle, lwd=2, lty=2, col=2)
}

convertLogParams <- function(paramSpecs = NULL){
  # Take data frame 'paramSpecs' and change all the parameters within it that are labelled as "log_XXX" to
  # be in standard space, i.e. use the exp() command on all appropriate values.
  # Returns a data frame of the same size as 'dat', with all 'log_' removed from the parameter names

  inpParamNames <- rownames(paramSpecs)
  # Ugliness ensues here as the parameter names in the control file may not match those in the output.
  # Must use a series of special checks to match up estimated parameters with names that contain
  #  parameter names that had priors and ignore those that don't.
  pattern <- "^log_([[:alnum:]]+)$"
  logp <- grep(pattern, inpParamNames)
  logInpNames <- inpParamNames[logp]
  logInpNames <- sub(pattern, "\\1", logInpNames) # Now the log param names have no "log_" in front of them
  logParamSpecs <- paramSpecs[logp,]
  # Special cases.... yuck
  logInpNames <- sub("avgrec","rbar",logInpNames)
  logInpNames <- sub("recinit","rinit",logInpNames)

  nonLogInpNames <- inpParamNames[-logp]
  # Special case... yuck
  nonLogInpNames <- sub("steepness","h",nonLogInpNames)
  nonLogParamSpecs <- paramSpecs[-logp,]

  for(param in 1:length(logInpNames)){
    # exp the log functions before sending them off for plotting
    logParamSpecs[param, 1] <- exp(logParamSpecs[param,1])
    logParamSpecs[param, 2] <- exp(logParamSpecs[param,2])
    logParamSpecs[param, 3] <- exp(logParamSpecs[param,3])
    logParamSpecs[param, 6] <- exp(logParamSpecs[param,6])
    # The second parameter in the prior is not logged
    logParamSpecs[param, 7] <- exp(logParamSpecs[param,7])
  }
  rownames(logParamSpecs) <- logInpNames
  paramSpecs <- rbind(nonLogParamSpecs, logParamSpecs)
  # paramSpecs now holds the input values for all parameters in regular (non-log) space with
  # names to reflect this (log_) has been removed from the names of the log parameters
  return(paramSpecs)
}

plotVariancePartitions <- function(mcmcData, burnthin = list(0,1), showtitle = TRUE, latexnames=FALSE){
  # Produce a grid of pairs plots for the variance parameters' posteriors
  # Assumes that there are an equal number of each rho and vartheta
  #  parameters, i.e. each group must have both rho and vartheta and
  #  they must be in the same column order in the matrix mcmcData.

	oldPar	<- par(no.readonly=T)
  on.exit(par(oldPar))

  val          <- getWinVal()
  burnin       <- burnthin[[1]]
  thinning     <- burnthin[[2]]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  # Create an empty parameter list and data frame for mcmc data and add each parameter to it
  rhoparams <- NULL
  vparams <- NULL

  # Get the names of all the output parameters for parsing
  names <- names(mcmcData)

  # Get rho parameters
  pattern <- "rho"
  rholoc <- grep(pattern, names)
  rhoparams <- c(rhoparams, names[rholoc])
  # Get vartheta parameters
  pattern <- "vartheta"
  varthetaloc <- grep(pattern, names)
  vparams <- c(vparams, names[varthetaloc])


  if(length(rhoparams) < 1 || length(vparams) < 1){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must contain at least one of each variance parameter (rho and vartheta).\n")
    return(NULL)
  }
  rhodat <- window(mcmc(as.ts(mcmcData[rhoparams[1]])), start = burnin, thin = thinning)
  vdat   <- window(mcmc(as.ts(mcmcData[vparams[1]])), start = burnin, thin = thinning)
  sig    <- rhodat * vdat
  tau    <- (1 - rhodat) * vdat
  # Make names for sig and tau for this group
  # Get the group name from the first element of the rho names
  rhoname <- names[rholoc][1]
  gname <- sub("rho_([[:alnum:]]+)","\\1",rhoname)
  namevec <- c(paste0("sig_",gname), paste0("tau_",gname))
  d <- data.frame(rhodat, vdat)
  d[,namevec] <- c(sig, tau)
  # Now the first group's variance parameters are correctly loaded into the data.frame
  if(length(rhoparams) > 1){
    for(param in 2:length(rhoparams)){
      rhodat <- window(mcmc(as.ts(mcmcData[rhoparams[param]])), start = burnin, thin = thinning)
      vdat   <- window(mcmc(as.ts(mcmcData[vparams[param]])), start = burnin, thin = thinning)
      sig    <- rhodat * vdat
      tau    <- (1 - rhodat) * vdat
      # Make names for sig and tau for this group
      # Get the group name from the first element of the rho names
      rhoname <- names[rholoc][param]
      gname   <- sub("rho_([[:alnum:]]+)","\\1",rhoname)
      namevec <- c(rhoparams[param], vparams[parma], paste0("sig_",gname), paste0("tau_",gname))
      d[,namevec] <- c(rhodat, vdat, sig, tau)
    }
  }
  pairs(d, pch=".", upper.panel=NULL, gap=0)

}

splitchain <- function(mc, nchains){
  # Split the given mcmc data (mc) into nchain mcmc objects and return an mcmc.list of them
  chainlist <- list()
  nrowseach <- floor(nrow(mc)/nchains)
  lower <- 1
  upper <- nrowseach
  for(chain in 1:nchains){
    chainlist[[chain]] <- mcmc(mc[lower:upper,])
    lower <- upper + 1
    upper <- lower + nrowseach - 1
  }
  return(chainlist)
}

plotGelman <- function(mcmcData, nchains = 2, burnthin=burnthin, showtitle = showtitle, latexnames=latexnames){
  # Plot the gelman-rubin convergence for these mcmcData

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burnin       <- burnthin[[1]]
  thinning     <- burnthin[[2]]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  # np is number of parameters
  np <- ncol(mcmcData)
  nside <- getRowsCols(np)
  par(mfrow = nside, oma = c(2,3,1,1), mai = c(0.2,0.4,0.3,0.2))
  mcmcData <- window(as.matrix(mcmcData), start=burnin, thin=thinning)
  for(param in 1:np){
    par(mar=.MCMC_MARGINS)
    mc <- as.matrix(mcmcData[,param])
    # Break chain into 'nchains' number of chains
    mc <- splitchain(mc, nchains)
    gelman.plot(mc, auto.layout=FALSE, xlab="", ylab="")
    name <- colnames(mcmcData)[param]
    regname <- name
    if(latexnames){
      name <- getLatexName(name)
    }
    title(name)
    #cat0("gelman.diag for parameter: ", regname)
    #print(gelman.diag(mc))
  }
  # Add legend in its own panel
  #plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
  #box()
  #legend(1,1,legend=c("Median","97.5%"),col=c("black","red"),lty=c(1,4), cex=0.6)
  mtext("Sample number in chain", side=1, line=1, outer=TRUE)
  mtext("Shrink factor", side=2, outer=TRUE)
}

gelman.plot <- function (x, bin.width = 10, max.bins = 50, confidence = 0.95, 
    transform = FALSE, autoburnin = TRUE, auto.layout = TRUE, 
    ask, col = 1:2, lty = 1:2, xlab = "last iteration in chain", 
    ylab = "shrink factor", type = "l", ...) 
{
    if (missing(ask)) {
        ask <- if (is.R()) {
            dev.interactive()
        }
        else {
            interactive()
        }
    }
    x <- as.mcmc.list(x)
    oldpar <- NULL
    on.exit(par(oldpar))
    if (auto.layout) 
        oldpar <- par(mfrow = set.mfrow(Nchains = nchain(x), 
            Nparms = nvar(x)))
    y <- gelman.preplot(x, bin.width = bin.width, max.bins = max.bins, 
        confidence = confidence, transform = transform, autoburnin = autoburnin)
    all.na <- apply(is.na(y$shrink[, , 1, drop = FALSE]), 2, 
        all)
    if (!any(all.na)) 
        for (j in 1:nvar(x)) {
            matplot(y$last.iter, y$shrink[, j, ], col = col, 
                lty = lty, xlab = xlab, ylab = ylab, type = type, 
                ...)
            abline(h = 1)
            ymax <- max(c(1, y$shrink[, j, ]), na.rm = TRUE)
            #leg <- dimnames(y$shrink)[[3]]
            xmax <- max(y$last.iter)
            #legend(xmax, ymax, legend = leg, lty = lty, bty = "n", 
            #    col = col, xjust = 1, yjust = 1)
            #title(main = varnames(x)[j])
            if (j == 1) 
                oldpar <- c(oldpar, par(ask = ask))
        }
    return(invisible(y))
}

gelman.preplot <-
  function (x, bin.width = bin.width, max.bins = max.bins,
            confidence = confidence, transform = transform,
            autoburnin = autoburnin) 
{
  x <- as.mcmc.list(x)
  if (niter(x) <= 50) 
    stop("Less than 50 iterations in chain")
  nbin <- min(floor((niter(x) - 50)/thin(x)), max.bins)
  binw <- floor((niter(x) - 50)/nbin)
  last.iter <- c(seq(from = start(x) + 50 * thin(x), by = binw * 
                     thin(x), length = nbin), end(x))
  shrink <- array(dim = c(nbin + 1, nvar(x), 2))
  dimnames(shrink) <- list(last.iter, varnames(x),
                           c("median", paste(50 * (confidence + 1), "%",
                                             sep = ""))
                           )
  for (i in 1:(nbin + 1)) {
    shrink[i, , ] <- gelman.diag(window(x, end = last.iter[i]), 
                                 confidence = confidence,
                                 transform = transform,
                                 autoburnin = autoburnin)$psrf
  }
  all.na <- apply(is.na(shrink[, , 1, drop = FALSE]), 2, all)
  if (any(all.na)) {
    cat("\n******* Error: *******\n")
    cat("Cannot compute Gelman & Rubin's diagnostic for any chain \n")
    cat("segments for variables", varnames(x)[all.na], "\n")
    cat("This indicates convergence failure\n")
  }
  return(list(shrink = shrink, last.iter = last.iter))
}
