###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate :
####Purpose    :
####Packages   :
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################

###############################################################################
## Step 01
## Variable inputs
###############################################################################
## Set the working directory
dir.main <- "c:/ss/estgrowth"

my.spp <- c("cod")
# Number of ss3sim iterations
my.totnum <- 1:5
# Logical whether or not to run ss3sim bias adjustment for log normal recdevs
my.bias <- FALSE

#How to install the package
ss3sim.install <- "local" #Can be "github", "local", "NULL"
ss3sim.branch <- "feature/data"


###############################################################################
## Step 02
## Load packages
###############################################################################
if(!is.null(ss3sim.install)){
	if(ss3sim.install == "github"){
		devtools::install_github("ss3sim", username = "ss3sim",
		                         ref = ss3sim.branch)
	}
	if(ss3sim.install == "local"){
		devtools::install("c:/ss/ss3sim")
	}
}
library(r4ss); library(ss3sim)
dir.sub <- file.path(dir.main, "test")
dir.cases <- file.path(dir.main, "casefiles")
dir.results <- file.path(dir.main, "results")
###############################################################################
## Step 03
## Model locations
###############################################################################
#Specify where the model files are
d <- file.path(system.file("extdata", package = "ss3sim"), "models")
  spp.grid <- expand.grid(my.spp, c("om", "em"))
  models <- file.path(d, apply(spp.grid, 1, paste, collapse = "-"))
  my.casefiles <- list(A = "agecomp", B = "bin", E = "E", F = "F", 
                     I = "index", L = "lcomp", R = "R")
  #If you add a casefile (in folder estgrowth\casefile) add the number to the
  #appropriate letter here. Mainly you will be adding letters to E
  scenarios <- expand_scenarios(
    cases = list(A = c(0, 1), B = c(0), E = c(0, 1, 2), F = c(0), I = c(0),
                 L = c(0, 1, 2), R = c(0)), species = my.spp)
#set working directory
dir.create(dir.sub, showWarnings = FALSE)
setwd(dir.sub)
#devtools::load_all("c:/ss/ss3sim")
#Run a single iteration of a given scenario
run_ss3sim(iterations = 1, scenarios = "A0-B0-E2-F0-I0-L0-R0-cod",
           case_folder = dir.cases, case_files = my.casefiles, 
           om_dir = file.path(d, "cod-om"), 
           em_dir = file.path(d, "cod-em"), bias_adjust = FALSE,
           ignore.stdout = TRUE)
unlink("A0-B0-E2-F0-I0-L0-R0-cod", recursive = TRUE)

#Use the following to run all combinations
for(s in seq_along(my.spp)){
    #finds the appropriate folder for each species
	use.om <- models[grep(paste(my.spp[s], "om", sep = "-"), models)]
	use.em <- models[grep(paste(my.spp[s], "em", sep = "-"), models)]
	#run scenarios that include the given species
    use.scen <- scenarios[grep(my.spp[s], scenarios)]

	run_ss3sim(iterations = my.totnum, scenarios = use.scen,
               case_folder = dir.cases, case_files = my.casefiles, 
               om_dir = use.om, em_dir = use.em, bias_adjust = my.bias,
               ignore.stdout = TRUE)
}


###############################################################################
## Step 
## Get results
###############################################################################
get_results_all(overwrite_files = TRUE, user_scenarios = scenarios)
scalars <- read.csv(dir(pattern = "r.csv", full.names = TRUE))
ts <- read.csv(dir(pattern = "s.csv", full.names = TRUE))