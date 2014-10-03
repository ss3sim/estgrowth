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
dir.sub <- file.path(dir.main, "test")
dir.cases <- file.path(dir.main, "caseFiles")
dir.results <- file.path(dir.main, "results")

my.spp <- c("cod")
my.totnum <- 1:5
my.bias <- FALSE
my.D <- c(0)
my.E <- c(0)
my.F <- c(0)
my.R <- c(0)
my.casefiles <- list(F = "F", 
                     D = c("index", "lcomp", "agecomp"), 
                     R = "R", E = "E")


ss3sim.install <- "local" #Can be "github", "local", "NULL"
ss3sim.branch <- "master"


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

###############################################################################
## Step 03
## Source functions
###############################################################################


###############################################################################
## Step 04
## Model locations
###############################################################################
d <- file.path(system.file("extdata", package = "ss3sim"), "models")
  spp.grid <- expand.grid(my.spp, c("om", "em"))
  models <- file.path(d, apply(spp.grid, 1, paste, collapse = "-"))
  scenarios <- expand_scenarios(cases = list(D = my.D, E = my.E,
                                F = my.F, R = my.R),
                                species = my.spp)
dir.create(dir.sub, showWarnings = FALSE)
setwd(dir.sub)

for(s in seq_along(my.spp)){
	use.om <- models[grep(paste(my.spp[s], "om", sep = "-"), models)]
	use.em <- models[grep(paste(my.spp[s], "em", sep = "-"), models)]
	use.scen <- scenarios[grep(my.spp[s], scenarios)]

	run_ss3sim(iterations = my.totnum, scenarios = use.scen,
               case_folder = file.path(dir.main, "casefiles"),
               case_files = my.casefiles, 
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