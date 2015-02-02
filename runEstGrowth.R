###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate :
####Purpose    :
####Packages   :
####Inputs     : This script assumes you have cloned ss3sim/growth_models to
####             your local machine.
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
#The following directory needs to be cloned prior to running.
dir.models <- "c:/ss/growth_models"
dir.dropbox <- "c:/users/kelli/dropbox/estgrowth"

my.spp <- c("cos", "fll")
# Number of ss3sim iterations
my.totnum <- 1:25
# Logical whether or not to run ss3sim bias adjustment for log normal recdevs
my.bias <- FALSE

## How to install the package

# Can be "github", "local", "NULL"
# ss3sim.install <- "github"
ss3sim.install <- "local"

ss3sim.branch <- "master"

###############################################################################
## Step 02
## Load packages
###############################################################################
if(!is.null(ss3sim.install)){
	if(ss3sim.install == "github"){
		devtools::install_github("ss3sim/ss3sim", ref = ss3sim.branch)
	}
	if(ss3sim.install == "local"){
		devtools::install("c:/ss/ss3sim")
	}
}
library(r4ss); library(ss3sim)
setwd(dir.models)
system("git fetch")
system("git rebase origin/master")
setwd(dir.main)
dir.sub <- file.path(dir.main, "test")
dir.cases <- file.path(dir.main, "casefiles")
dir.results <- file.path(dir.main, "results")
###############################################################################
## Step 03
## Model locations
###############################################################################
source(file.path(dir.main, "lib", "createcasefiles.r"))

#Specify where the model files are
d <- file.path(system.file("extdata", package = "ss3sim"), "models")
  spp.grid <- expand.grid(my.spp, c("om", "em"))
  models <- file.path(dir.models, apply(spp.grid, 1, paste, collapse = "-"))
  my.casefiles <- list(A = "agecomp", E = "E", F = "F", D = "mlacomp",
    I = "index", L = "lcomp", R = "R") #,
#    S = c(toupper(rev(letters)[1:6])), M = "M")

  internal <- expand_scenarios(cases =
    list(E = 0:1, L = 0:2, A = 0:3, D = 0,
         F = 1, I = 0, R = 0), species = my.spp)

  external <- expand_scenarios(cases =
    list(E = 2, L = 0:2, A = 0:2, D = 1:2,
         F = 1, I = 0, R = 0), species = my.spp)

#set working directory
dir.create(dir.sub, showWarnings = FALSE)
setwd(dir.sub)
devtools::load_all("c:/ss/ss3sim")

# # # Run a single iteration of a test scenario
test <- "A0-D1-E2-F1-I0-L0-R0-cos"
unlink(test, recursive = TRUE)
run_ss3sim(iterations = 1, scenarios = test,
           case_folder = dir.cases, case_files = my.casefiles,
           om_dir = models[1],
           em_dir = models[2], bias_adjust = FALSE,
           ignore.stdout = TRUE, show.output.on.console = FALSE)
unlink(test, recursive = TRUE)

# Set up running in parallel
library(doParallel)
library(foreach)
registerDoParallel(cores = 3)
getDoParWorkers() # check

#Use the following to run all combinations
for(s in seq_along(my.spp)){
  #finds the appropriate folder for each species
	use.om <- models[grep(paste(my.spp[s], "om", sep = "-"), models)]
	use.em <- models[grep(paste(my.spp[s], "em", sep = "-"), models)]
	#run scenarios that include the given species
  use.scen <- c(internal, external)
  use.scen <- use.scen[sapply(my.spp[s], grepl, use.scen)]
	run_ss3sim(iterations = my.totnum, scenarios = use.scen,
               case_folder = dir.cases, case_files = my.casefiles,
               om_dir = use.om, em_dir = use.em, bias_adjust = my.bias,
               ignore.stdout = TRUE, show.output.on.console = FALSE,
               parallel = FALSE)
  # Should also maybe set ss_mode = "optimized"
}

###############################################################################
## Step
## Get results
###############################################################################
get_results_all(overwrite_files = FALSE)
scalars <- read.csv(dir(pattern = "r.csv", full.names = TRUE))
ts <- read.csv(dir(pattern = "s.csv", full.names = TRUE))

file.copy(dir(pattern = "r.csv", full.names = TRUE),
          file.path(dir.dropbox, "scalars.csv"),
          overwrite = TRUE, copy.mode = TRUE)
file.copy(dir(pattern = "s.csv", full.names = TRUE),
          file.path(dir.dropbox, "ts.csv"),
          overwrite = TRUE, copy.mode = TRUE)

source(file.path(dir.main, "lib", "results.r"))