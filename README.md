____
# iscam-gui

iscam-gui is an R gui which is used to run and show output of various
fisheries iScam models with respect to each other.
The typical use of this it to quickly show results of modelling
sensitivity analyses together.  The software was
designed by Chris Grandin.

_____________________________________________________________


## History
This software was originally used to look at various sensitivities
for the TINSS (This Is Not Stock Synthesis) modelling platform.
The scope for TINSS broadened and the project became iScam. The original creator
of iScam is Steve Martell. His project is hosted on GitHub here: (https://github.com/smartell/iScam)
iScam has been used for many fish stocks and the need for
an organized structure is necessary to avoid possible confusion of
model outputs among sensitivity runs, so this project was born.

## Acknowledgements
Thanks to the following people involved in the development of this software:

* Steve Martell (iScam source and initial GIT template)
* Robyn Forrest (iScam model development and GUI user/tester)
* Kendra Holt   (iScam user)

---

## Prerequisites
* iScam project from cgrandin's repository: (https://github.com/cgrandin/iScam)
* R (version 2.15 or later)
* R Packages (and their dependencies)
	* PBSmodelling

---
## Setting up new Scenarios:

- **Scenarios** is a directory with a specific directory structure.
  It is important to maintain this directory structure because R
  uses relative paths for maintaining inputs and outputs to automate
  much of the running of iScam and its outputs.

- To add a new model (called a *Scenario*) to iscam-gui, copy an
  entire directory containing an iScam model including the
  starter file (iScam.dat), data file, control file, projection file, and any other
  files the model requires into the **Scenarios** directory.

- There should also be a file called **ScenarioInfo.txt** in each Scenario directory
  which must contain three values representing:
    1. *Sensitivity Group* - A number which represents which group this scenario belongs to.
    2. *Color for plotting* - the R number color for plotting this scenario..
    3. *Plotting Order* - NOT YET IMPLEMENTED where 1 is highest. If there are multiple values, they will be ordered by name alphabetically.

    These value can be changed inside the GUI as well,
  and when that change is made, the file will be modified to contain the new value.
  Scenarios with the same *Sensitivity Group* will be plotted together for comparisons.
  If the file is missing, a default will be created by the software.

- Upon launching, iscam-gui will automatically try to load all directories present
  in the *Scenarios* directory and you will be able to run them from within the GUI.

- If you want to look at the length-weight and length-age (VonB) relationships, you should have a
  directory called *Biodata* at the same level as the *Scenarios* directory. This must hold the
  lengthweight.tpl and vonb.tpl which are ADMB programs used to estimate fit for each.

---

## Data structures:

- There are three global lists which hold all the data used by iscam-gui:
  - **op** - This is a list of lists, one element for each *Scenario*.  names(op) will return the directory names of your Scenarios.

  - **sens** - This is a list of *Sensitivity Group*, one for each unique *Sensitivity Group*.  names(sens) will return NULL
since the groups are nameless.
  - **bio** - This is a list of length two, containing the input data and output parameter estimates for the
length-weight and VonB realationships.


- There are three objects containing the valid surveys. These are used by functions in iscam-gui-load-biodata.r. They are:
  surveyList, a data frame of key/description pairs, surveyKeys, the keys, and surveyValues, the descriptions.

The following depicts the **op**, **sens**, and **bio** object structures. Indentations reflect sub-object structure.
See source files iscam-gui-load-scenarios.r and iscam-gui-load-biodata.r to see how these lists are populated or to add new elements.

    op[[N]] - Each unique scenario number N contains the following
      op[[N]]$names - Full path names for the files and directories used in iscam-gui
        op[[N]]$names$scenario         - Name of scenario
        op[[N]]$names$dir              - Name of scenario directory
        op[[N]]$names$figDir           - Name of Figures directory
        op[[N]]$names$tableDir         - Name of Tables directory
        op[[N]]$names$biodata          - Name of the biological data file (default NULL)
        op[[N]]$names$data             - Name of the data file
        op[[N]]$names$control          - Name of the control file
        op[[N]]$names$projection       - Name of the projection file
        op[[N]]$names$log              - Name of command line output from model run file
        op[[N]]$names$par              - Name of the PAR file
        op[[N]]$names$warnings         - Name of the warning file
        op[[N]]$names$sensitivityGroup - Name of the sensitivity group file
        op[[N]]$names$lastCommandRun   - Name of the file containing the last model commands run
      op[[N]]$inputs - All inputs into iscam and iscam-gui
        op[[N]]$inputs$sensitivityGroup - Sensitivity group number (scenarios with the same number will be plotted together)
        op[[N]]$inputs$color            - Color for plotting. Read in from the Scenario info file.
        op[[N]]$inputs$order            - Order for plotting, 1 is plotted first. If multiple values are the same, it will sort by name.
        op[[N]]$inputs$starter          - Starter file contents
        op[[N]]$inputs$data - Data file contents (iscam model DAT file)
          op[[N]]$inputs$data$narea       - Number of areas
          op[[N]]$inputs$data$ngroup      - Number of groups
          op[[N]]$inputs$data$nsex        - Number of sexes
          op[[N]]$inputs$data$syr         - First year of data
          op[[N]]$inputs$data$nyr         - Last year of data
          op[[N]]$inputs$data$sage        - Youngest age
          op[[N]]$inputs$data$nage        - Oldest age or plus group
          op[[N]]$inputs$data$ngear       - Total number of gears (includes commercial fisheries and surveys)
          op[[N]]$inputs$data$alloc       - Allocation for each gear (0 = no allocation, 1=allocation)
          op[[N]]$inputs$data$linf        - Asymptotic length (Spies and Turlock)
          op[[N]]$inputs$data$k           - Brody growth coefficient from AFSC
          op[[N]]$inputs$data$to          - Theoretical age at zero length from AFSC
          op[[N]]$inputs$data$lwscal      - Scalar in length-weight allometry, Spies and Turlock (g and cm)
          op[[N]]$inputs$data$lwpow       - Power parameter in length-weight allometry, Spies and Turnock
          op[[N]]$inputs$data$age50       - age at 50% maturity (approx with log(3.0)/k) from AFSC
          op[[N]]$inputs$data$sd50        - std at 50% maturity (CV ~ 0.1)
          op[[N]]$inputs$data$usemat      - Use maturity vector (0=don't use, 1=use)
          op[[N]]$inputs$data$matvec      - Maturity vector. If usemat=0, then this should be a single 0, not a vector
          op[[N]]$inputs$data$nctobs      - Number of catch observations
          op[[N]]$inputs$data$catch       - matrix of catch input into model
          op[[N]]$inputs$data$nit         - Number of abundance indices
          op[[N]]$inputs$data$nitnobs     - Vector of number of index observations for each nit
          op[[N]]$inputs$data$indices     - A list of matrices of length nit of the index observations
          op[[N]]$inputs$data$nagears     - Number of gears with age composition data
          op[[N]]$inputs$data$nagearsvec  - Vector of number of age comp observations for each gear in nagears
          op[[N]]$inputs$data$nagearssage - Vector of youngest ages for each gear in nagears
          op[[N]]$inputs$data$nagearsnage - Vector of oldest ages (or plus groups) for each gear in nagears
          op[[N]]$inputs$data$eff         - Effictive sample size for each gear in nagears
          op[[N]]$inputs$data$agecomps    - List of matrices of length nagears of the age comps
          op[[N]]$inputs$data$nwttab      - Number of weight-at-age tables
          op[[N]]$inputs$data$nwtobs      - Number of rows of weight-at-age data. Use 0 if there aren't any to follow
          op[[N]]$inputs$data$waa         - Matrix of the weight-at-age data. NULL if nwtobs = 0
          op[[N]]$inputs$data$eof         - End of file marker, must be 999 if the file was read in correctly
        op[[N]]$inputs$control - Control file contents (iscam CTL file)
          op[[N]]$inputs$control$npar         - Number of parameters
          op[[N]]$inputs$control$param        - Matrix of input parameter values, phases, and priors
          op[[N]]$inputs$control$as           - Matrix of age/size composition, 1 column for each of nagears
          op[[N]]$inputs$control$sel          - Matrix of selectivity parameters, one column for each ngear ($data$ngear)
          op[[N]]$inputs$control$syrtimeblock - Vector of start years for each time block, 1 for each ngear
          op[[N]]$inputs$control$nits         - Number of surveys (must be the same as $data$nit)
          op[[N]]$inputs$control$survq        - Matrix of priors for survey q, one column for each nits
          op[[N]]$inputs$control$misc         - Matrix of 1 column of Miscellaneous controls used in the model
          op[[N]]$inputs$control$eof          - End of file marker, must be 999 if the file was read in correctly
        op[[N]]$inputs$projection - Projection file contents (iscam model PFC file)
          op[[N]]$inputs$projection$ntac          - Length of catch vector used for projections (catch streams)
          op[[N]]$inputs$projection$tacvec        - Vector of the catch streams to project, length ntac
          op[[N]]$inputs$projection$ncntrloptions - Number of control options to follow
          op[[N]]$inputs$projection$cntrloptions  - Matrix of 1 column of the control options. M, Fec, and Rec are here.
          op[[N]]$inputs$projection$eof           - End of file marker, must be 999 if the file was read in correctly
        op[[N]]$inputs$numParams   - The number of parameters, extracted from the first line of PAR file.
        op[[N]]$inputs$objFunValue - The objective function value, extracted from the first line of PAR file.
        op[[N]]$inputs$maxGradient - The maximum gradient, extracted from the first line of PAR file.
        op[[N]]$inputs$log - Log file created by runCurrScenario() contents (if it exists)
          op[[N]]$inputs$log$isMPD       - The loaded model run was an MPD run (TRUE/FALSE)
          op[[N]]$inputs$log$isMCMC      - The loaded model run was an MCMC run (TRUE/FALSE)
          op[[N]]$inputs$log$hasMCeval   - The loaded model run was an MCMC run for which mceval has been run (TRUE/FALSE)
          op[[N]]$inputs$log$finishTimes - The finish time for the model run.
          op[[N]]$inputs$log$numWarnings - Number of warnings generated by the model run
          op[[N]]$inputs$log$lastCommandLine - A list containing the command line switches used for a scenario.
            op[[N]]$inputs$log$lastCommandLine$maxfn   - Maximum function calls
            op[[N]]$inputs$log$lastCommandLine$mcmc    - Run Markov Chain Monte Carlo with this number in the chain
            op[[N]]$inputs$log$lastCommandLine$mcsave  - Save frequency for an mcmc run
            op[[N]]$inputs$log$lastCommandLine$mcseed  - Seed for random number generator for Markov Chain Conte Carlo
            op[[N]]$inputs$log$lastCommandLine$mno     - The maximum number of independent variables
            op[[N]]$inputs$log$lastCommandLine$mcscale - Rescale step size for first n evaluations
            op[[N]]$inputs$log$lastCommandLine$maxph   - Increase the maximum phase number to n
            op[[N]]$inputs$log$lastCommandLine$mcrb    - Modify the covariance matrix to reduce extremely high correlation (0<=n<=1)
            op[[N]]$inputs$log$lastCommandLine$mcprobe - Use probing strategy for mcmc with factor n
            op[[N]]$inputs$log$lastCommandLine$gbs     - Set GRADSTACK_BUFFER_SIZE TO n
            op[[N]]$inputs$log$lastCommandLine$crit    - Set gradient magnitude convergence criterion to n
            op[[N]]$inputs$log$lastCommandLine$ams     - Set arrmblsize to n (ARRAY_MEMBLOCK_SIZE)
            op[[N]]$inputs$log$lastCommandLine$phase   - Start minimization in phase n
            op[[N]]$inputs$log$lastCommandLine$cbs     - Set CMPDIF_BUFFER_SIZE TO n
            op[[N]]$inputs$log$lastCommandLine$mdl     - Set the maximum number of dvariables to n
      op[[N]]$fileSuccess - A list containing TRUE if files exist and were loaded correctly, FALSE otherwise
        op[[N]]$fileSuccess$starter          - Starter file
        op[[N]]$fileSuccess$data             - Data file
        op[[N]]$fileSuccess$control          - Control file
        op[[N]]$fileSuccess$projection       - Projection file
        op[[N]]$fileSuccess$mpd              - MPD data files
        op[[N]]$fileSuccess$mpdForecast      - MPD forecasting file
        op[[N]]$fileSuccess$mcmc             - MCMC output files
        op[[N]]$fileSuccess$log              - Log file created by runCurrScenario()
        op[[N]]$fileSuccess$par              - PAR file
        op[[N]]$fileSuccess$sensitivityGroup - SensitivityGroup file
        op[[N]]$fileSuccess$lastCommandRun   - Last command run file
      op[[N]]$outputs - A list containing all the outputs from the model run
        op[[N]]$outputs$mpd  - Data Frame containing the ouput of the mpd model run
        op[[N]]$outputs$mcmc - Data frame containing the output of the mcmc model run
          op[[N]]$outputs$mcmc$params - Output parameter posteriors from the MCEVAL phase
          op[[N]]$outputs$mcmc$sbt    - Spawning biomass posteriors from the MCEVAL phase
          op[[N]]$outputs$mcmc$rt     - Recruitment posteriors from the MCEVAL phase
          op[[N]]$outputs$mcmc$ft     - Fishing mortality posteriors from the MCEVAL phase
        op[[N]]$outputs$par  - Parameter file contents (iscam model PAR file)
        op[[N]]$outputs$par$theta1                  - log_ro parameter estimate
        op[[N]]$outputs$par$theta2                  - h (steepness) parameter estimate
        op[[N]]$outputs$par$theta3                  - log_m parameter estimate
        op[[N]]$outputs$par$theta4                  - log_avgrec parameter estimate
        op[[N]]$outputs$par$theta5                  - log_recinit parameter estimate
        op[[N]]$outputs$par$theta6                  - rho parameter estimate
        op[[N]]$outputs$par$theta7                  - vartheta parameter estimate
        op[[N]]$outputs$par$sel_par1                - Vector of selectivity parameters, 1 for each nit
        op[[N]]$outputs$par$sel_par2                - Vector of selectivity parameters, 1 for each nit
        op[[N]]$outputs$par$sel_par3                - Vector of selectivity parameters, 1 for each nit
        op[[N]]$outputs$par$log_ft_pars             - Vector of fishing mortality parameters, one for each year
        op[[N]]$outputs$par$init_log_rec_devs       - Vector of recruitment devs for non-virgin (fished) B0
        op[[N]]$outputs$par$log_rec_devs            - Vector of estimated recruitment deviations, 1 for each year
        op[[N]]$outputs$par$log_m_nodes             - Vector of estimated nodes for natural mortality
        op[[N]]$outputs$par$log_age_tau21           - Parameter 1 for age
        op[[N]]$outputs$par$log_age_tau22           - Parameter 2 for age
        op[[N]]$outputs$par$phi11                   - Parameter 1 for nit 1 for phi
        op[[N]]$outputs$par$phi12                   - Parameter 1 for nit 2 for phi
        op[[N]]$outputs$par$phi21                   - Parameter 2 for nit 1 for phi
        op[[N]]$outputs$par$phi22                   - Parameter 2 for nit 2 for phi
        op[[N]]$outputs$par$log_degrees_of_freedom1 - For nit 1
        op[[N]]$outputs$par$log_degrees_of_freedom2 - For nit 2
        op[[N]]$outputs$par$gamma_r                 - Single gamma parameter
        op[[N]]$outputs$par$numParams               - The number of parameters in the model
        op[[N]]$outputs$par$objFunValue             - Objective function value returned by the run
        op[[N]]$outputs$par$maxGradient             - Maximum gradient from the run. This should be a very small number.
        op[[N]]$outputs$par$retros                  - List of Retrospectives (with filenames as $name) - REP file contents. If NULL, no retros
    sens[[M]] - each unique sensitivity group number M contains the following
      sens[[M]][1] - A vector of the indicies within the op list of the Scenarios that are currently in sensitivity group M
    bio - Contains length/weight and VonB data and parameter estimates for the species of concern.
      bio$lw[[1]] - Length/weight data and parameter estimates for males or combined if there is no second element.
        bio$lw[[1]][[1]] - Two-column matrix of length in mm (column 1) and weight in g (column 2).
        bio$lw[[1]][[2]] - Vector of the two parameter estimates for this sex.
      bio$lw[[2]] - Length/weight data and parameter estimates for females (optional).
        bio$lw[[2]][[1]] - Two-column matrix of length in mm (column 1) and weight in g (column 2).
        bio$lw[[2]][[2]] - Vector of the two parameter estimates for this sex.
      bio$vonb[[1]] - Length/age data and parameter estimates for males or combined if there is no second element.
        bio$vonb[[1]][[1]] - Two-column matrix of length in mm (column 1) and age (column 2).
        bio$vonb[[1]][[2]] - Vector of the two parameter estimates for this sex.
      bio$vonb[[2]] - Length/weight data and parameter estimates for females (optional).

---

## Useful GIT commands can be found at my other repository used for learning GIT:

      https://github.com/cgrandin/git-workshop

---

