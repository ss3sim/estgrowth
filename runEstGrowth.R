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

#For Christine's machine
main.path <- "c:/users/christine stawitz/documents/github"
dir.main <- file.path(main.path,"estgrowth")
dir.dropbox <- "c:/users/christine stawitz/documents/dropbox/ExtGrowthResults"

#For Kelli's machine
## Set the working directory
if (Sys.info()["user"] == "kelli") {
  dir.main <- "c:/ss/estgrowth"
  dir.dropbox <- "c:/users/kelli/dropbox/estgrowth"
}


# Which species do you want to use?
my.spp <- c("cod", "flatfish", "yellow")
my.spp <- my.spp[order(my.spp)]

my.totnum <- 1:50
# Logical whether or not to run ss3sim bias adjustment to gain information
# about recruitment from years with more information
my.bias.num <- 0

# Register the number of cores you want to use
my.corenum <- 1

# Logical whether in testing mode or not
testingmode <- FALSE

###############################################################################
## Step 02
## Load packages
###############################################################################
library(devtools)
install_github("r4ss/r4ss", ref = "master")
install_github("ss3sim/ss3sim", ref = "master")
install_github("ss3sim/ss3models", ref = "master")

library(doParallel); library(foreach); library(r4ss);
library(ss3models); library(ss3sim);

dir.models <- system.file("models", package = "ss3models")

dir.sub <- file.path(dir.main, "test")
dir.cases <- file.path(dir.main, "cases")
dir.results <- file.path(dir.main, "results")

# Copy case files from github growth_models repository to the local folder
# for casefiles.
setwd(dir.main)
file.copy(system.file("cases", package = "ss3models"), ".", recursive = TRUE)

###############################################################################
## Step 03
## Model locations
###############################################################################
# file below need true values of M for each species
truem <- vector(length = length(my.spp))
mfiles <- file.path(system.file("models", package = "ss3models"),
    dir(system.file("models", package = "ss3models")), "om", "ss3.ctl")
keep <- which(apply(unlist(sapply(paste0(my.spp, "/"), grepl, mfiles)),
  1, sum) > 0)
for (ind in keep) {
  om <- mfiles[ind]
  pars <- SS_parlines(om)
  truem[which(ind == keep)] <- pars[grep("NatM", pars$Label), "INIT"]
}

source(file.path(dir.main, "lib", "createcasefiles.r"))

# Specify where the model files are
models <- sapply(c("om", "em"), function (x) {
  file.path(system.file("models", package = "ss3models"),
  dir(system.file("models", package = "ss3models"))[keep], x)
})
my.casefiles <- list(A = "agecomp", C = "calcomp", D = "mlacomp",
                     E = "E", F = "F",
                     I = "index", L = "lcomp") #,
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
  # # # Run a single iteration of a test scenario
  test <- paste("A31-C0-D0-E1-F0-I0-L31", my.spp, sep = "-")
  unlink(test, recursive = TRUE)
  for (ind in seq_along(my.spp)) {
    my.om <- models[ind, "om"]
    my.em <- models[ind, "em"]
    run_ss3sim(iterations = 1, scenarios = test[ind],
             case_folder = dir.cases, case_files = my.casefiles,
             om_dir = my.om, em_dir = my.em, bias_adjust = FALSE,
             ignore.stdout = TRUE, show.output.on.console = FALSE,
             user_recdevs = recdevs)
  }

  recdevs <- matrix(0, nrow = 100, ncol = 10000)
} else {
  recdevs <- NULL
}
  if (exists("test")) {
    removeme <- file.path(dir.sub, test)
    ignore <- sapply(removeme, unlink, recursive = TRUE)
  }


# Set up running in parallel if specified
registerDoParallel(cores = my.corenum)
getDoParWorkers() # check

#Use the following to run all combinations
for(s in seq_along(my.spp)){
  #finds the appropriate folder for each species
	use.om <- models[s, "om"]
	use.em <- models[s, "em"]
	#run scenarios that include the given species
  use.scen <- torun[sapply(my.spp[s], grepl, torun)]
	run_ss3sim(iterations = my.totnum, scenarios = use.scen,
    case_folder = dir.cases, case_files = my.casefiles,
    om_dir = use.om, em_dir = use.em,
    bias_adjust = ifelse(my.bias.num > 0, TRUE, FALSE), bias_nsim = my.bias.num,
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
file.copy(dir(pattern = "scalar", full.names = TRUE),
          file.path(dir.dropbox, "ts.csv"),
          overwrite = TRUE, copy.mode = TRUE)

source(file.path(dir.main, "lib", "results.r"))
