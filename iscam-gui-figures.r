.writePlots <- function(silent = .SILENT){
  # write all figures for the given scenario to disk

  val      <- getWinVal()
  scenario <- val$entryScenario
  isMCMC   <- op[[scenario]]$inputs$log$isMCMC
  figDir   <- op[[scenario]]$names$figDir

  if(isMCMC){
    ## plot mcmc model runs
    out <- op[[scenario]]$outputs$mcmc
    ## plotTS(scenarios, type=.DEPLETION_FIGURE, ylim=c(0,1.4), png=png, verbose=verbose)
  }else{
    ## plot mpd model runs
    out <- op[[scenario]]$outputs$mpd
  }

  ## Generate r4ss plots
  SS_plots(out, plot=c(1:18,20:27), png=TRUE, printfolder=figDir, uncertainty=F)

}

