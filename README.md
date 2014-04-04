____
# iscam-gui

iscam-gui is an R gui which is used to run and show output of various
fisheries iScam models with respect to each other.
The typical use of this it to quickly show results of modelling
sensitivity analyses together.  The software was
designed by Chris Grandin.

    Integrated Statistical Catch Age GUI (iscam-gui)
                        VERSION 1.0

            Created by Chris Grandin on 2014-03-21

       Source code: https://github.com/cgrandin/iscam-gui
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
#Obtaining iscam-gui:
Obtaining the latest version of iscam-gui should be done by forking from the [gitHub](https://github.com/cgrandin/iscam-gui) repository

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
  much of the running of SS and its outputs.

- To add a new model (called a *Scenario*) to isdcam-gui, copy an
  entire directory containing an iScam model with executable (iscam.exe for Windows or iscam for Linux or MAC),
  starter file, data file, control file, projection file, and any other
  files the model requires into the **Scenarios** directory.

- There must also be a file called **ScenarioInfo.txt** in each Scenario directory
  which must contain three values representing:
    1. *Sensitivity Group* - A number which represents which group this scenario belongs to.
    2. *Color for plotting* - the R number color for plotting this scenario..
    3. *Plotting Order* - where 1 is highest. If there are multiple values, they will be ordered by name alphabetically.

    These value can be changed inside the GUI as well,
  and when that change is made, the file will be modified to contain the new value.
  Scenarios with the same *Sensitivity Group* will be plotted together for comparisons.

- Upon launching, iscam-gui will automatically try to load all directories present
  in the *Scenarios* directory and you will be able to run them from within the GUI.

---

## Data structures:

- There are two global lists which hold all the data used by iscam-gui:
  - **op** - This is a list of lists, one element for each *Scenario*.  names(op) will return the directory names of your Scenarios.

  - **sens** - This is a list of *Sensitivity Group*, one for each unique *Sensitivity Group*.  names(sens) will return NULL
since the groups are nameless.

The following depicts the **op** and **sens** object structure. Indentations reflect sub-object structure.
See source file iscam-gui-load-scenarios.r to see how these lists are populated or to add new elements.

    op[[N]] - each scenario number N contains the following
      op[[N]]$names (full paths)
        op[[N]]$names$scenario         - Name of scenario
        op[[N]]$names$dir              - Name of scenario directory
        op[[N]]$names$figDir           - Name of Figures directory
        op[[N]]$names$tableDir         - Name of Tables directory
        op[[N]]$names$data             - Name of the data file
        op[[N]]$names$control          - Name of the control file
        op[[N]]$names$projection       - Name of the projection file
        op[[N]]$names$log              - Name of command line output from model run file
        op[[N]]$names$par              - Name of the PAR file
        op[[N]]$names$warnings         - Name of the warning file
        op[[N]]$names$sensitivityGroup - Name of the sensitivity group file
        op[[N]]$names$lastCommandRun   - Name of the file containing the last model commands run
      op[[N]]$inputs
        op[[N]]$inputs$sensitivityGroup - Sensitivity group number (scenarios with the same number will be plotted together)
        op[[N]]$inputs$color            - Color for plotting. Read in from the Scenario info file.
        op[[N]]$inputs$order            - Order for plotting, 1 is highest. If multiple values are the same, sort by name.
        op[[N]]$inputs$starter          - Starter file contents
        op[[N]]$inputs$data      - Data file contents (iscam model DAT file)
          op[[N]]$inputs$data$narea          - Number of areas
          op[[N]]$inputs$data$ngroup         - Number of groups
          op[[N]]$inputs$data$nsex           - Number of sexes
          op[[N]]$inputs$data$syr            - First year of data
          op[[N]]$inputs$data$nyr            - Last year of data
          op[[N]]$inputs$data$sage           - Youngest age
          op[[N]]$inputs$data$nage           - Oldest age or plus group
          op[[N]]$inputs$data$ngear          - Total number of gears (includes commercial fisheries and surveys)
          op[[N]]$inputs$data$alloc          - Allocation for each gear (0 = no allocation, 1=allocation)
          op[[N]]$inputs$data$linf           - Asymptotic length (Spies and Turlock)
          op[[N]]$inputs$data$k              - Brody growth coefficient from AFSC
          op[[N]]$inputs$data$to             - Theoretical age at zero length from AFSC
          op[[N]]$inputs$data$lwscal         - Scalar in length-weight allometry, Spies and Turlock (g and cm)
          op[[N]]$inputs$data$lwpow          - Power parameter in length-weight allometry, Spies and Turnock
          op[[N]]$inputs$data$age50          - age at 50% maturity (approx with log(3.0)/k) from AFSC
          op[[N]]$inputs$data$sd50           - std at 50% maturity (CV ~ 0.1)
          op[[N]]$inputs$data$usemat         - Use maturity vector (0=don't use, 1=use)
          op[[N]]$inputs$data$matvec         - Maturity vector. If usemat=0, then this should be a single 0, not a vector
          op[[N]]$inputs$data$nctobs         - Number of catch observations
          op[[N]]$inputs$data$catch          - matrix of catch input into model
          op[[N]]$inputs$data$nit            - Number of abundance indices
          op[[N]]$inputs$data$nitnobs        - Vector of number of index observations for each nit
          op[[N]]$inputs$data$indices        - A list of matrices of length nit of the index observations
          op[[N]]$inputs$data$nagears        - Number of gears with age composition data
          op[[N]]$inputs$data$nagearsvec     - Vector of number of age comp observations for each gear in nagears
          op[[N]]$inputs$data$nagearssage    - Vector of youngest ages for each gear in nagears
          op[[N]]$inputs$data$nagearsnage    - Vector of oldest ages (or plus groups) for each gear in nagears
          op[[N]]$inputs$data$eff            - Effictive sample size for each gear in nagears
          op[[N]]$inputs$data$agecomps       - List of matrices of length nagears of the age comps
          op[[N]]$inputs$data$nwttab         - Number of weight-at-age tables
          op[[N]]$inputs$data$nwtobs         - Number of rows of weight-at-age data. Use 0 if there aren't any to follow
          op[[N]]$inputs$data$waa            - Matrix of the weight-at-age data. NULL if nwtobs = 0
          op[[N]]$inputs$data$eof            - End of file marker, must be 999 if the file was read in correctly
        op[[N]]$inputs$control   - Control file contents (iscam model CTL file)
          op[[N]]$inputs$control$npar           - Number of parameters
          op[[N]]$inputs$control$param          - Matrix of input parameter values, phases, and priors
          op[[N]]$inputs$control$as             - Matrix of age/size composition, 1 column for each of nagears
          op[[N]]$inputs$control$sel            - Matrix of selectivity parameters, one column for each ngear ($data$ngear)
          op[[N]]$inputs$control$syrtimeblock   - Vector of start years for each time block, 1 for each ngear
          op[[N]]$inputs$control$nits           - Number of surveys (must be the same as $data$nit)
          op[[N]]$inputs$control$survq          - Matrix of priors for survey q, one column for each nits
          op[[N]]$inputs$control$misc           - Matrix of 1 column of Miscellaneous controls used in the model
          op[[N]]$inputs$control$eof            - End of file marker, must be 999 if the file was read in correctly
        op[[N]]$inputs$projection - Projection file contents (iscam model PFC file)
          op[[N]]$inputs$projection$ntac           - Length of catch vector used for projections (catch streams)
          op[[N]]$inputs$projection$tacvec         - Vector of the catch streams to project, length ntac
          op[[N]]$inputs$projection$ncntrloptions  - Number of control options to follow
          op[[N]]$inputs$projection$cntrloptions   - Matrix of 1 column of the control options. M, Fec, and Rec are here.
          op[[N]]$inputs$projection$eof            - End of file marker, must be 999 if the file was read in correctly
        op[[N]]$inputs$numParams       - The number of parameters, extracted from the first line of PAR file.
        op[[N]]$inputs$objFunValue      - The objective function value, extracted from the first line of PAR file.
        op[[N]]$inputs$maxGradient      - The maximum gradient, extracted from the first line of PAR file.
        op[[N]]$inputs$log              - Log file created by runCurrScenario() contents (if it exists)
          op[[N]]$inputs$log$isMPD          - The loaded model run was an MPD run (TRUE/FALSE)
          op[[N]]$inputs$log$isMCMC         - The loaded model run was an MCMC run (TRUE/FALSE)
          op[[N]]$inputs$log$hasMCeval      - The loaded model run was an MCMC run for which mceval has been run (TRUE/FALSE)
          op[[N]]$inputs$log$finishTimes    - The finish time for the model run.
          op[[N]]$inputs$log$numWarnings    - Number of warnings generated by the model run
          op[[N]]$inputs$log$lastCommandLine  - A list containing the command line switches used for a scenario.
            op[[N]]$inputs$log$lastCommandLine$maxfn          - Maximum function calls
            op[[N]]$inputs$log$lastCommandLine$mcmc           - Run Markov Chain Monte Carlo with this number in the chain
            op[[N]]$inputs$log$lastCommandLine$mcsave         - Save frequency for an mcmc run
            op[[N]]$inputs$log$lastCommandLine$mcseed         - Seed for random number generator for Markov Chain Conte Carlo
            op[[N]]$inputs$log$lastCommandLine$mno            - The maximum number of independent variables
            op[[N]]$inputs$log$lastCommandLine$mcscale        - Rescale step size for first N evaluations
            op[[N]]$inputs$log$lastCommandLine$maxph          - Increase the maximum phase number to N
            op[[N]]$inputs$log$lastCommandLine$mcrb           - Modify the covariance matrix to reduce extremely high correlation (0<=N<=1)
            op[[N]]$inputs$log$lastCommandLine$mcprobe        - Use probing strategy for mcmc with factor N
            op[[N]]$inputs$log$lastCommandLine$gbs            - Set GRADSTACK_BUFFER_SIZE TO N
            op[[N]]$inputs$log$lastCommandLine$crit           - Set gradient magnitude convergence criterion to N
            op[[N]]$inputs$log$lastCommandLine$ams            - Set arrmblsize to N (ARRAY_MEMBLOCK_SIZE)
            op[[N]]$inputs$log$lastCommandLine$phase          - Start minimization in phase N
            op[[N]]$inputs$log$lastCommandLine$cbs            - Set CMPDIF_BUFFER_SIZE TO N
            op[[N]]$inputs$log$lastCommandLine$mdl            - Set the maximum number of dvariables to N
      op[[N]]$fileSuccess        - A list containing TRUE if files exist and were loaded correctly, FALSE otherwise
        op[[N]]$fileSuccess$starter          - Starter file
        op[[N]]$fileSuccess$data             - Data file
        op[[N]]$fileSuccess$control          - Control file
        op[[N]]$fileSuccess$projection       - Projection file
        op[[N]]$fileSuccess$mpd              - MPD data files
        op[[N]]$fileSuccess$mpdForecast      - MPD forecating file
        op[[N]]$fileSuccess$mcmc             - MCMC output files
        op[[N]]$fileSuccess$log              - Log file created by runCurrScenario()
        op[[N]]$fileSuccess$par              - PAR file
        op[[N]]$fileSuccess$sensitivityGroup - SensitivityGroup file
        op[[N]]$fileSuccess$lastCommandRun   - Last command run file
      op[[N]]$outputs
        op[[N]]$outputs$mpd              - Data Frame containing the ouput of the mpd model run
        op[[N]]$outputs$mcmc             - Data frame containgin the output of the mcmc model run
        op[[N]]$outputs$par              - Parameter file contents (iscam model PAR file)
                           $theta1         - log_ro parameter estimate
                         $theta2         - steepness parameter estimate
                         $theta3         - log_m parameter estimate
                         $theta4         - log_avgrec parameter estimate
                         $theta5         - log_recinit parameter estimate
                         $theta6         - rho parameter estimate
                         $theta7         - vartheta parameter estimate
                         $sel_par1       - Vector of selectivity parameters, 1 for each nit
                         $sel_par2       - Vector of selectivity parameters, 1 for each nit
                         $sel_par3       - Vector of selectivity parameters, 1 for each nit
                         $log_ft_pars    - Vector of fishing mortality parameters, one for each year
                         $init_log_rec_devs - Vector of recruitment devs for non-virgin (fished) B0
                         $log_rec_devs   - Vector of estimated recruitment deviations, 1 for each year
                         $log_m_nodes    - Vector of estimated nodes for natural mortality
                         $log_age_tau21  - Parameter 1 for age
                         $log_age_tau22  - Parameter 2 for age
                         $phi11          - Parameter 1 for nit 1 for phi
                         $phi12          - Parameter 1 for nit 2 for phi
                         $phi21          - Parameter 2 for nit 1 for phi
                         $phi22          - Parameter 2 for nit 2 for phi
                         $log_degrees_of_freedom1 - For nit 1
                         $log_degrees_of_freedom2 - For nit 2
                         $gamma_r        - Single gamma parameter
                         $numParams      - The number of parameters in the model
                         $objFunValue    - Objective function value returned by the run
                         $maxGradient    - Maximum gradient from the run. This should be a very small number.
                       $retros           - List of Retrospective data (with filenames as $name) - REP file contents. If NULL, no retros.
    sens[[N]] - each Sensitivity Group number N contains the following
            $names              - A vector of names of the Scenarios that are currently in sensitivity group N
            $summary            - The list returned by SSsummarize() function run on all Scenarios names in $names
            $isMCMC             - If any of the models in the sensitivity group are mcmc runs, this will be TRUE, otherwise FALSE

## Cloning the repository

First, *Fork* this project on the GitHub site.
Obtain a copy of github software for windows [here](http://windows.github.com/), then
Open 'Git Shell' and run the following:

	git clone https://github.com/{your-GitHub-user-name}/iscam-gui

## Commiting changes locally

After making a small number of changes, you commit them to your local
repository by issuing the command:

	git commit -a -m "Descriptive message"

## Commiting local changes to the master on github

After committing locally, you commit changes to the master repository on the
github site by issuing the command:

	git push

---

## Useful GIT commands can be found at my other repository used for learning GIT:

      https://github.com/cgrandin/git-workshop

---

