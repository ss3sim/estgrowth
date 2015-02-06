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
## Variable inputs: objects in Step 01 may need alteration prior to running
###############################################################################
## Set the working directory
dir.main <- "c:/ss/estgrowth"
#The following directory needs to be cloned prior to running.
dir.models <- "c:/ss/growth_models"
dir.ss3sim <- "c:/ss/ss3sim"
dir.dropbox <- "c:/users/kelli/dropbox/estgrowth"

# Which species do you want to use?
my.spp <- c("cos")#, "fll")
# Number of ss3sim iterations
my.totnum <- 1:25
# Logical whether or not to run ss3sim bias adjustment to gain information
# about recruitment from years with more information
my.bias <- FALSE
my.bias.num <- 5

# Register the number of cores you want to use
my.corenum <- 1

## How to install the package
# Can be "github", "local", "NULL"
# ss3sim.install <- "github"
ss3sim.install <- "github"
ss3sim.branch <- "master"

# Logical whether in testing mode or not
testingmode <- TRUE

###############################################################################
## Step 02
## Load packages
###############################################################################
# perform checks to make sure computer is set up prior to running model
if (!is.null(ss3sim.install)){
	if(ss3sim.install == "github"){
    if (as.logical(system("ping www.github.com"))) {
      stop(paste("A valid internet connection does not exist, please change",
                 "ss3sim.install to 'local'"))
    }
		devtools::install_github("ss3sim/ss3sim", ref = ss3sim.branch)
	}
	if (ss3sim.install == "local"){
    if (!file.exists(dir.ss3sim)) {
      stop(paste("The ss3sim directory", dir.ss3sim, "does not exist on this",
                 "computer, please clone the remote repository."))
    }
		devtools::install(dir.ss3sim)
	}
}

library(doParallel); library(foreach); library(r4ss); library(ss3sim);

# Update models from github repository on local machine
if (file.exists(dir.models)) {
  setwd(dir.models)
  system("git fetch")
  status <- system("git status", intern = TRUE)
    if (status[1] != "# On branch master") {
      checkout <- system("git checkout master", intern = TRUE)
    } if (checkout[1] == "Switched to branch 'master'") {
      system("git rebase origin/master")
    } else {
      stop(paste("More than likely your working directory",
        dir.models, "is not clean and you cannot switch to master."))
    }
} else {
  stop(paste("The ss3sim directory", dir.models, "does not exist on this",
             "computer, please clone the remote repository."))
}
dir.sub <- file.path(dir.main, "test")
dir.cases <- file.path(dir.main, "casefiles")
dir.results <- file.path(dir.main, "results")

# Copy case files from github growth_models repository to the local folder
# for casefiles.
files2copy <- dir(file.path(dir.models, "cases"), full.names = TRUE)
done <- sapply(files2copy, function(x) {
  file.copy(from = x, to = gsub(file.path(dir.models, "cases"), dir.cases, x),
            overwrite = TRUE)
})
if (any(!done)) {
  warning("casefiles not copied from remote repository")
}

###############################################################################
## Step 03
## Model locations
###############################################################################
setwd(dir.main)
source(file.path(dir.main, "lib", "createcasefiles.r"))

# Specify where the model files are
d <- file.path(system.file("extdata", package = "ss3sim"), "models")
  spp.grid <- expand.grid(my.spp, c("om", "em"))
  models <- file.path(dir.models, apply(spp.grid, 1, paste, collapse = "-"))
  my.casefiles <- list(A = "agecomp", C = "calcomp", D = "mlacomp",
                       E = "E", F = "F",
                       I = "index", L = "lcomp", R = "R") #,
# Below are cases that we are not currently using
# S == selectivity case files (dome)
# M == time-varying natural mortality
#    S = c(toupper(rev(letters)[1:6])), M = "M")

# Import scenario list from excel file
scenariosfile <- file.path(dir.main, "lib", "scenarios.csv")
scenarios <- read.table(scenariosfile, sep = ",", header = TRUE)
torun <- unlist(lapply(scenarios$complete, paste0, my.spp))

#set working directory
dir.create(dir.sub, showWarnings = FALSE)
setwd(dir.sub)


if (testingmode) {
  devtools::load_all(dir.ss3sim)
  # # # Run a single iteration of a test scenario
  test <- "A31-C20-D20-E2-F1-I0-L31-R0-cos"
  unlink(test, recursive = TRUE)
  run_ss3sim(iterations = 1, scenarios = test,
             case_folder = dir.cases, case_files = my.casefiles,
             om_dir = models[1],
             em_dir = models[2], bias_adjust = FALSE,
             ignore.stdout = TRUE, show.output.on.console = FALSE)
  recdevs <- matrix(0, nrow = 100, ncol = 10000)
}
  if (exists("test")) unlink(test, recursive = TRUE)


# Set up running in parallel if specified
registerDoParallel(cores = my.corenum)
getDoParWorkers() # check

#Use the following to run all combinations
for(s in seq_along(my.spp)){
  #finds the appropriate folder for each species
	use.om <- models[grep(paste(my.spp[s], "om", sep = "-"), models)]
	use.em <- models[grep(paste(my.spp[s], "em", sep = "-"), models)]
	#run scenarios that include the given species
  use.scen <- use.scen[sapply(my.spp[s], grepl, torun)]
	run_ss3sim(iterations = my.totnum, scenarios = use.scen,
               case_folder = dir.cases, case_files = my.casefiles,
               om_dir = use.om, em_dir = use.em,
               bias_adjust = my.bias, bias_nsim = my.bias.num,
               ignore.stdout = TRUE, show.output.on.console = FALSE,
               parallel = ifelse(getDoParWorkers() > 1, TRUE, FALSE),
               user_recdevs = recdevs, user_recdevs_warn = FALSE)
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