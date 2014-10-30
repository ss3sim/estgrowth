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
###############################################################################
#### Step
#### Set up folder structure
###############################################################################
###############################################################################

d <- system.file("extdata", package = "ss3sim")
case_folder <- file.path(dir.main, "casefiles")

###############################################################################
###############################################################################
#### Step
#### Profile Fmsy for cod with length based selectivity
#### where selectivity is specified using a double normal
###############################################################################
###############################################################################
om <- file.path(dir.main, "/models/col-om")
em <- file.path(dir.main, "/models/col-em")

profile_fmsy(om, results_out = "fmsy", 
             start = 0.05, end = 0.25, by = 0.001)

data <- read.csv(file.path("fmsy", "fmsy.txt"), sep = " ", header = TRUE)
Fmsy <- data$fValues[which.max(data$eqCatch)]
catch.lower90 <- max(data$eqCatch) * 0.90

F90l <- data$fValues[sapply(catch.lower90, function(x){
    which.min(abs(x - data$eqCatch[1:which.max(data$eqCatch)]))
    })]
F90u <- data$fValues[sapply(catch.lower90, function(x){
    which.min(abs(x - data$eqCatch[which.max(data$eqCatch) : 
                                   length(data$eqCatch)])) + 
    which.max(data$eqCatch) - 1
    })]

###############################################################################
###############################################################################
#### Step
#### Basic run
###############################################################################
###############################################################################
# run_ss3sim(iterations = 1, scenarios = "D0-E0-F0-R0-M0-col",
#   case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe")
# unlink("D0-E0-F0-R0-M0-col", recursive = TRUE) # clean up