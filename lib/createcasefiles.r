###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-02-05
####Purpose    : Create case files for growth estimation
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
#### Set working directories
###############################################################################
###############################################################################
wd.curr <- getwd()
setwd(dir.cases)
start <- 1
end   <- 100

start.fishery <- 26
years.rup <- 40

start.survey  <- start + 75
start.fishery <- start + 25
freq.survey  <- 2
freq.fishery <- c(10, 4)

# Information regarding sample frequency
# Amount to cut data by (e.g., if 2 take every other point)
reducer <- 2
# Number of years randomly picked for mlacomps
nmlayears <- 2
# Number of years randomly picked for calcomps
ncalyears <- 2

# Information regarding sample intensity
# Currently not using the lower sample size as Ono et al. (2014)
# showed sample size really did not matter
# lcomp and age comp sample sizes
high <- 40
low <- 10

# Estimate CVs or not
estCVs <- TRUE

###############################################################################
###############################################################################
#### Step
#### Cases that are not currently being used
###############################################################################
###############################################################################
# Change asymptotic selectivity to dome shaped in OM
# Not currently using, but available, list with a vector of 7 parameter values to
# add to selectivity, each species needs a vector. Amount is how much to
# add or subtract from the double normal currently programmed as asymptotic
# dome <- list(c(51.5 - 50.8, -4 - -3, 5.2 - 5.1, 8 - 15, 0, 0))
# amount of random noise in selectivity for each spp - pertains to parameter 1
tvs <- rep(0.1, length = length(my.spp))

# Sequence true natural morality values for each species by 10 - 190%
# These values will be used to change the EM values of fixed natural mortality
# to add misspecification in the EM.
if (!exists("truem")) {
  stop(paste("True values of natural mortality are needed for each species",
    "before you can run this script."))
}
mrange <- lapply(truem, function(x) {
  temp <- x * seq(.5, 1.50, by = 0.1)
  temp[-which(temp == x)]
})



###############################################################################
###############################################################################
#### Step
#### Calculate years that things happen based on inputs above
###############################################################################
###############################################################################
years.fish <- end + start - start.fishery

# Number of years the fishery ramps up before starting two way trip
all.surv <- seq(start.survey, end, by = freq.survey)
all.fish <- c(seq(start.fishery, start.fishery + 20, by = freq.fishery[1]),
              seq(start.fishery + 30, end, by = freq.fishery[2]))

less <- sapply(c("surv", "fish"), function(x) {
                realword <- paste("all", x, sep = ".")
                vals <- eval(parse(text = realword))
                vals[seq.int(1L, length(vals), reducer)]
               })

years.txt <- lapply(list(as = all.surv, af = all.fish,
                         ls = less$surv, lf = less$fish),
                    function(x) {
                      paste0("c(", paste(x, collapse = ", "), ")")
                    })

###############################################################################
###############################################################################
#### Step
#### Functions to create casefiles
###############################################################################
###############################################################################
# E change_e, change parameter estimation
writeE <- function(name, int, phase, species, case) {
sink(paste0("E", case, "-", species, ".txt"))
    cat("natM_type; 1Parm \nnatM_n_breakpoints; NULL \n",
        "natM_lorenzen; NULL \n", sep = "")
    testfornatm <- grepl("NatM_p_1_Fem", name)
    if (any(testfornatm)) {
        cat("natM_val; c(", int[testfornatm], ", ", phase[testfornatm],
            ")\n", sep = "")
    } else {
      cat("natM_val; c(NA, NA) \n", sep = "")
    }
    if (all(testfornatm)) {
        cat("par_name; NULL \n", "par_int; NULL \n", "par_phase; NULL \n", sep = "")
      } else{
        cat("par_name; c(\"",
            paste0(name[!testfornatm], collapse = "\", \""), "\")\n", sep = "")
        if (all(is.na(int))) {
            cat("par_int; c(",
            paste0(int[!testfornatm], collapse = ", "), ")\n",
            "par_phase; c(",
            paste0(phase[!testfornatm], collapse = ", "), ")\n",
            sep = "")
        } else {
            cat("par_int; c(\"",
            paste0(int[!testfornatm], collapse = "\", \""), "\")\n",
            "par_phase; c(\"",
            paste0(phase[!testfornatm], collapse = "\", \""), "\")\n",
            sep = "")
        }
      }
      cat("forecast_num; 0\nrun_change_e_full; TRUE\n", sep = "")
  sink()
}

###############################################################################
## Step
## Sequence along all species listed in my.spp
###############################################################################
for (spp in seq_along(my.spp)) {

###############################################################################
###############################################################################
#### Step
#### change_e: case "E"
###############################################################################
###############################################################################
# Switch to the below code if we want to also estimate the CV parameters
if (estCVs) {
  allgrowth <- c("L_at_Amin", "L_at_Amax", "VonBert_K", "CV_young", "CV_old")
} else {
  allgrowth <- c("L_at_Amin", "L_at_Amax", "VonBert_K")
}

growthint <- rep(NA, length(allgrowth))
growthphase <- rep(-1, length(allgrowth))
counter <- 0

# All parameters are fixed at their true OM values
  writeE(allgrowth, growthint, growthphase, my.spp[spp], counter)
# All parameters are estimated
  writeE(NULL, "NA", "NA", my.spp[spp], counter + 1)
# All parameters are externally estimated
  writeE(allgrowth, rep("change_e_vbgf", length(allgrowth)),
         growthphase, my.spp[spp], counter + 2)
  writeE(c("L_at_Amin", "L_at_Amax", "VonBert_K"),
    rep("change_e_vbgf", 3), rep(-1, 3), my.spp[spp], counter + 3)

# Misspecify M
  for(i in seq_along(mrange[[spp]])) {
    writeE(c("NatM_p_1_Fem"), mrange[[spp]][i], -1, my.spp[spp], 19 + i)
    writeE(c("NatM_p_1_Fem", allgrowth),
           c(mrange[[spp]][i], rep("change_e_vbgf", length(allgrowth))),
           c(-1, growthphase), my.spp[spp], 9 + i)
  }


###############################################################################
###############################################################################
#### Step
#### Close the loop for all species
###############################################################################
###############################################################################
}

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################
setwd(wd.curr)