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
if (Sys.info()["user"] == "kfjohns") {
  dir.main <- "h:/estgrowth"
  dir.dropbox <- "h:/estgrowth"
}


# Which species do you want to use?
my.spp <- c("hake", "flatfish", "yellow")
my.spp <- my.spp[order(my.spp)]

my.totnum <- 1:50
# Logical whether or not to run ss3sim bias adjustment to gain information
# about recruitment from years with more information
my.bias.num <- 10

# Register the number of cores you want to use
my.corenum <- 3

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
for (ind in seq_along(my.spp)) {
  om <- file.path(ss3model(my.spp[ind], "om"), "ss3.ctl")
  pars <- SS_parlines(om)
  truem[ind] <- pars[grep("NatM", pars$Label), "INIT"]
}

source(file.path(dir.main, "lib", "createcasefiles.r"))

my.casefiles <- list(A = "agecomp", C = "calcomp", D = "mlacomp",
                     E = "E", F = "F",
                     I = "index", L = "lcomp") #,
# Below are cases that we are not currently using
# S == selectivity case files (dome)
# M == time-varying natural mortality
#    S = c(toupper(rev(letters)[1:6])), M = "M")

# Import scenario list from excel file
setwd("lib"); system("make"); setwd("..")
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
    use.om <- ss3model(my.spp[ind], "om")
    use.em <- ss3model(my.spp[ind], "em")
    run_ss3sim(iterations = 50, scenarios = test[ind],
      case_folder = dir.cases, case_files = my.casefiles,
      om_dir = use.om, em_dir = use.em, bias_adjust = FALSE,
      ignore.stdout = TRUE, show.output.on.console = FALSE)
  }
}
  if (length(ls(pattern = "test$", envir=.GlobalEnv)) > 0) {
    removeme <- file.path(dir.sub, test)
    ignore <- sapply(removeme, unlink, recursive = TRUE)
  }


# Set up running in parallel if specified
registerDoParallel(cores = my.corenum)
getDoParWorkers() # check

#Use the following to run all combinations
for(s in seq_along(my.spp)){
  #finds the appropriate folder for each species
    use.om <- ss3model(my.spp[s], "om")
    use.em <- ss3model(my.spp[s], "em")
	#run scenarios that include the given species
  use.scen <- torun[sapply(my.spp[s], grepl, torun)]
	run_ss3sim(iterations = my.totnum, scenarios = use.scen,
    case_folder = dir.cases, case_files = my.casefiles,
    om_dir = use.om, em_dir = use.em,
    bias_adjust = ifelse(my.bias.num > 0, TRUE, FALSE), bias_nsim = my.bias.num,
    ignore.stdout = TRUE, show.output.on.console = FALSE,
    parallel = ifelse(getDoParWorkers() > 1, TRUE, FALSE))
  # Should also maybe set ss_mode = "optimized"
}

###############################################################################
## Step
## Get results
###############################################################################
get_results_all(overwrite_files = FALSE,
  parallel = ifelse(getDoParWorkers() > 1, TRUE, FALSE))
scalars <- read.csv(dir(pattern = "r.csv", full.names = TRUE))
ts <- read.csv(dir(pattern = "s.csv", full.names = TRUE))

file.copy(dir(pattern = "r.csv", full.names = TRUE),
          file.path(dir.dropbox, "scalars.csv"),
          overwrite = TRUE, copy.mode = TRUE)
file.copy(dir(pattern = "scalar", full.names = TRUE),
          file.path(dir.dropbox, "ts.csv"),
          overwrite = TRUE, copy.mode = TRUE)

source(file.path(dir.main, "lib", "results.r"))

setwd(dir.main)
