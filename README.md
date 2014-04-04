____
# iscam-gui

iscam-gui is an R gui which is used to run and show output of various
fisheries iScam models with respect to each other.
The typical use of this it to quickly show results of modelling
sensitivity analyses together.  The software was
designed by Chris Grandin.

    Integrated Statistical Catch Age GUI (iscam-gui)
                        VERSION 1.0
           Last modified: Fri Mar 21 2014

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
            $names (full paths)
              $scenario         - Name of scenario
              $dir              - Name of scenario directory
              $figDir           - Name of Figures directory
              $tableDir         - Name of Tables directory
              $data             - Name of the data file
              $control          - Name of the control file
              $projection       - Name of the projection file
              $log              - Name of command line output from model run file
              $par              - Name of the PAR file
              $warnings         - Name of the warning file
              $sensitivityGroup - Name of the sensitivity group file
              $lastCommandRun   - Name of the file containing the last model commands run
            $inputs
              $sensitivityGroup - Sensitivity group number (scenarios with the same number will be plotted together)
              $color            - Color for plotting. Read in from the Scenario info file.
              $order            - Order for plotting, 1 is highest. If multiple values are the same, sort by name.
              $starter          - Starter file contents
              $data             - Data file contents (iscam model DAT file)
                $narea          - Number of areas
                $ngroup         - Number of groups
                $nsex           - Number of sexes
                $syr            - First year of data
                $nyr            - Last year of data
                $sage           - Youngest age
                $nage           - Oldest age or plus group
                $ngear          - Total number of gears (includes commercial fisheries and surveys)
                $alloc          - Allocation for each gear (0 = no allocation, 1=allocation)
                $linf           - Asymptotic length (Spies and Turlock)
                $k              - Brody growth coefficient from AFSC
                $to             - Theoretical age at zero length from AFSC
                $lwscal         - Scalar in length-weight allometry, Spies and Turlock (g and cm)
                $lwpow          - Power parameter in length-weight allometry, Spies and Turnock
                $age50          - age at 50% maturity (approx with log(3.0)/k) from AFSC
                $sd50           - std at 50% maturity (CV ~ 0.1)
                $usemat         - Use maturity vector (0=don't use, 1=use)
                $matvec         - Maturity vector. If usemat=0, then this should be a single 0, not a vector
                $nctobs         - Number of catch observations
                $catch          - matrix of catch input into model
                $nit            - Number of abundance indices
                $nitnobs        - Vector of number of index observations for each nit
                $indices        - A list of matrices of length nit of the index observations
                $nagears        - Number of gears with age composition data
                $nagearsvec     - Vector of number of age comp observations for each gear in nagears
                $nagearssage    - Vector of youngest ages for each gear in nagears
                $nagearsnage    - Vector of oldest ages (or plus groups) for each gear in nagears
                $eff            - Effictive sample size for each gear in nagears
                $agecomps       - List of matrices of length nagears of the age comps
                $nwttab         - Number of weight-at-age tables
                $nwtobs         - Number of rows of weight-at-age data. Use 0 if there aren't any to follow
                $waa            - Matrix of the weight-at-age data. NULL if nwtobs = 0
                $eof            - End of file marker, must be 999 if the file was read in correctly
              $control          - Control file contents (iscam model CTL file)
                $npar           - Number of parameters
                $param          - Matrix of input parameter values, phases, and priors
                $as             - Matrix of age/size composition, 1 column for each of nagears
                $sel            - Matrix of selectivity parameters, one column for each ngear ($data$ngear)
                $syrtimeblock   - Vector of start years for each time block, 1 for each ngear
                $nits           - Number of surveys (must be the same as $data$nit)
                $survq          - Matrix of priors for survey q, one column for each nits
                $misc           - Matrix of 1 column of Miscellaneous controls used in the model
                $eof            - End of file marker, must be 999 if the file was read in correctly
              $projection       - Projection file contents (iscam model PFC file)
                $ntac           - Length of catch vector used for projections (catch streams)
                $tacvec         - Vector of the catch streams to project, length ntac
                $ncntrloptions  - Number of control options to follow
                $cntrloptions   - Matrix of 1 column of the control options. M, Fec, and Rec are here.
                $eof            - End of file marker, must be 999 if the file was read in correctly
              $numParams        - The number of parameters, extracted from the first line of PAR file.
              $objFunValue      - The objective function value, extracted from the first line of PAR file.
              $maxGradient      - The maximum gradient, extracted from the first line of PAR file.
              $log              - Log file created by runCurrScenario() contents (if it exists)
                $isMPD          - The loaded model run was an MPD run (TRUE/FALSE)
                $isMCMC         - The loaded model run was an MCMC run (TRUE/FALSE)
                $hasMCeval      - The loaded model run was an MCMC run for which mceval has been run (TRUE/FALSE)
                $finishTimes    - The finish time for the model run.
                $numWarnings    - Number of warnings generated by the model run
              $covar            - Use covariance file from SS.  Used in SS_outputs and SS_plots. (TRUE/FALSE)
              $lastCommandLine  - A list containing the command line switches used for a scenario.
                $maxfn          - Maximum function calls
                $mcmc           - Run Markov Chain Monte Carlo with this number in the chain
                $mcsave         - Save frequency for an mcmc run
                $mcseed         - Seed for random number generator for Markov Chain Conte Carlo
                $mno            - The maximum number of independent variables
                $mcscale        - Rescale step size for first N evaluations
                $maxph          - Increase the maximum phase number to N
                $mcrb           - Modify the covariance matrix to reduce extremely high correlation (0<=N<=1)
                $mcprobe        - Use probing strategy for mcmc with factor N
                $gbs            - Set GRADSTACK_BUFFER_SIZE TO N
                $crit           - Set gradient magnitude convergence criterion to N
                $ams            - Set arrmblsize to N (ARRAY_MEMBLOCK_SIZE)
                $phase          - Start minimization in phase N
                $cbs            - Set CMPDIF_BUFFER_SIZE TO N
                $mdl            - Set the maximum number of dvariables to N
            $fileSuccess        - A list containing TRUE if files exist and were loaded correctly, FALSE otherwise
              $starter          - Starter file
              $data             - Data file
              $control          - Control file
              $projection       - Projection file
              $mpd              - MPD data files
              $mpdForecast      - MPD forecating file
              $mcmc             - MCMC output files
              $log              - Log file created by runCurrScenario()
              $forecast         - Forecast file
              $par              - PAR file
              $sensitivityGroup - SensitivityGroup file
              $lastCommandRun   - Last command run file
            $outputs
              $mpd              - Data Frame containing the ouput of the mpd model run
              $mcmc             - Data frame containgin the output of the mcmc model run
              $par              - Parameter file contents (iscam model PAR file)
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

