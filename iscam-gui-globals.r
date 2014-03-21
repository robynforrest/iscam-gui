#*******************************************************************************
# ss-explore-globals.r
# This file holds all global variable for the ss-explore source.
#*******************************************************************************

# Project name
.PROJECT_NAME                    <- "ss-explore"
.MAIN_FUNCTION_CALL              <- "sse"

# Editor
.EDITOR                          <- file.path("C:","Progra~1","emacs-22.1","bin","runemacs.exe")

# Directory names
.DATAFILE_DIR_NAME               <- paste(.PROJECT_NAME,"-datafile-gui",sep="")
.SCENARIOS_DIR_NAME              <- file.path("..","Scenarios")
.SENS_FIGURES_DIR_NAME           <- file.path("..","SensitivityFigures")
.FIGURES_DIR_NAME                <- "Figures"
.TABLES_DIR_NAME                 <- "Tables"

# R source files
.UTILITIES_SOURCE                <- paste(.PROJECT_NAME,"-utilities.r",sep="")
.LOAD_SCENARIOS_SOURCE           <- paste(.PROJECT_NAME,"-load-scenarios.r",sep="")
.FILE_CONTROL_SOURCE             <- paste(.PROJECT_NAME,"-file-control.r",sep="")
.FIGURES_SOURCE                  <- paste(.PROJECT_NAME,"-figures.r",sep="")
.FIGURES_BIOLOGY_SOURCE          <- paste(.PROJECT_NAME,"-figures-biology.r",sep="")
.FIGURES_SELEX_SOURCE            <- paste(.PROJECT_NAME,"-figures-selex.r",sep="")
.FIGURES_TIMESERIES_SOURCE       <- paste(.PROJECT_NAME,"-figures-timeseries.r",sep="")
.FIGURES_CATCH_SOURCE            <- paste(.PROJECT_NAME,"-figures-catch.r",sep="")

# R4SS Sources modified for ss-explore.  The rest are from package r4ss
.SS_READ_CTL                     <- "SS_readctl.R"
.SS_PLOTS                        <- "SS_plots.R"
.SS_PLOT_BIOLOGY                 <- "SSplotBiology.R"
.SS_PLOT_SELEX                   <- "SSplotSelex.R"
.SS_GET_MCMC                     <- "SSgetMCMC.R"
.SS_PLOT_COMPARISONS             <- "SSplotComparisons.R"
.SS_PLOT_RETRORECRUITS           <- "SSplotRetroRecruits.R"
.SS_PLOT_TIMESERIES              <- "SSplotTimeseries.R"
.SS_READ_STARTER                 <- "SS_readstarter.R"
.SS_READ_DAT                     <- "SS_readdat.R"
.SS_OUTPUT                       <- "SS_output.R"
.SS_SUMMARIZE                    <- "SSsummarize.R"
.SS_MOUNTAINS                    <- "mountains.R"
.SS_TABLE_COMPARISONS            <- "SStableComparisons.R"

# GUI definition files (see PBSModelling package)
.MAIN_GUI_DEF_FILE               <- "ss-explore-gui-specs.txt"

# SS file names
.STARTER_FILE_NAME               <- "starter.ss"
.CONTROL_FILE_NAME               <- "control.ss"
.DATA_FILE_NAME                  <- "data.ss"
.FORECAST_FILE_NAME              <- "forecast.ss"
.SS_EXE_BASE_NAME                <- "ss3"
.SS_EXE_FILE_NAME                <- paste(.SS_EXE_BASE_NAME,".exe",sep="")
.SS_EXE_FILE_NAME_FULL_PATH      <- file.path("..","executable",.SS_EXE_FILE_NAME)
.PAR_FILE_NAME                   <- paste(.SS_EXE_BASE_NAME,".par",sep="")
.LAST_COMMAND_RUN_FILE_NAME      <- "lastCommandRun.rdata"  # Contains a list of the command line switches used in a run.

# Sensitivity file name (for grouping sensitivities together on plots)
.SCENARIO_INFO_FILE_NAME         <- "ScenarioInfo.txt"

# GUI Header labels
.SCENARIO_LIST_LABEL             <- "Scenario List"
.SENSITIVITY_GROUP_LABEL         <- "Group"
.PLOT_COLOR_LABEL                <- "Color"
.PLOT_ORDER_LABEL                <- "Order"

# Messages
.TELL_USER_HOW_TO_START_GUI      <- "Type sse() to start ss-explore GUI\n"
.TELL_USER_ABOUT_GUI_ARGUMENTS   <- paste("Optional ",.PROJECT_NAME," arguments: ",.MAIN_FUNCTION_CALL,"(reloadScenarios = FALSE, silent = FALSE, copyModelExecutables = FALSE)\n\n",sep="")

# Model run command line outputs
.WARNING_FILE_NAME               <- "warning.sso"
.LOG_FILE_NAME                   <- "runoutput.log"  # This is the name of the logfile which holds all command line output.
.DOS_PIPE_STDOUT                 <- "1>"
.DOS_PIPE_STDERR                 <- "2>&1"
.DOS_APPEND_STDOUT               <- "1>>"
.DOS_APPEND_STDERR               <- "2>>&1"
.DOS_PIPE_TO_LOG                 <- paste(.DOS_PIPE_STDOUT, .LOG_FILE_NAME, .DOS_PIPE_STDERR)
.DOS_APPEND_TO_LOG               <- paste(.DOS_APPEND_STDOUT, .LOG_FILE_NAME, .DOS_APPEND_STDERR)

# SS output file lists
.POSTERIOR_FILE_NAME             <- "posteriors.sso"
.DERIVED_POSTERIOR_FILE_NAME     <- "derived_posteriors.sso"
.REPORT_FILE_NAME                <- "Report.sso"
.COMP_FILE_NAME                  <- "CompReport.sso"
.COVAR_FILE_NAME                 <- "covar.sso"
.FORECAST_REPORT_FILE_NAME       <- "Forecast-report.sso"
.WT_FILE_NAME                    <- "wtatage.ss_new"
.WARNING_FILE_NAME               <- "warning.sso"
.MPD_OUTPUT_FILES                <- c(.LAST_COMMAND_RUN_FILE_NAME,
                                      paste(.SS_EXE_BASE_NAME,".b*",sep=""),
                                      paste(.SS_EXE_BASE_NAME,".p*",sep=""), # Also removes .par file
                                      paste(.SS_EXE_BASE_NAME,".r*",sep=""), # Also removes .rep file
                                      paste(.SS_EXE_BASE_NAME,".log",sep=""),
                                      "*.cov",
                                      "*.cor",
                                      "*.dep",
                                      "*.eva",
                                      "*.hes",
                                      "*.par",
                                      "*.std",
                                      "*.ss_new",
                                      .WARNING_FILE_NAME,
                                      "variance",
                                      "SIS_table.sso",
                                      "checkup.sso",
                                      .COMP_FILE_NAME,
                                      .COVAR_FILE_NAME,
                                      "CumReport.sso",
                                      .DERIVED_POSTERIOR_FILE_NAME,
                                      "echoinput.sso",
                                      "eigv.rpt",
                                      "fmin.log",
                                      .FORECAST_REPORT_FILE_NAME,
                                      "ParmTrace.sso",
                                      .POSTERIOR_FILE_NAME,
                                      "posterior_vectors.sso",
                                      "rebuild.sso",
                                      .REPORT_FILE_NAME,
                                      "runnumber.ss",
                                      .LOG_FILE_NAME,
                                      # MCMC ouputs only
                                      paste(.SS_EXE_BASE_NAME,".ecm",sep=""),
                                      paste(.SS_EXE_BASE_NAME,".hst",sep=""),
                                      paste(.SS_EXE_BASE_NAME,".mc*",sep=""),
                                      "sims",
                                      # Executable
                                      .SS_EXE_FILE_NAME)

# Figure types
.PNG                             <- FALSE
.DEPLETION_FIGURE                <- 1

# Plotting
.RESOLUTION                      <- 300
.WIDTH                           <- 7
.HEIGHT                          <- 7
.UNITS                           <- "in"
.VERBOSE                         <- FALSE

# For the ScenarioInfo.txt file only:
.SENS_TEXT                       <- "# Sensitivity Group"
.DEFAULT_PLOT_COLOR              <- "1"
.PLOT_COLOR_TEXT                 <- "# Plotting color"
.DEFAULT_SENS_GROUP              <- "1"
.ORDER_TEXT                      <- "# Plotting order"
.DEFAULT_PLOT_ORDER              <- "1"

# Other globals
.FUNEVALS                        <- 150
.PCHCODE                         <- 16
.BANNER                          <- "-----------------------------------------------------------------------------------\n"
.BANNER2                         <- "===================================================================================\n"

# Sneaky, keeo a global for if the plotting is currently on or not
.PLOT_IS_LIVE                    <- TRUE
