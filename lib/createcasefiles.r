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

###############################################################################
###############################################################################
#### Step
#### Cases that are not currently being used
###############################################################################
###############################################################################
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
for(counter in c(0, 100)) {
  allgrowth <- c("L_at_Amin", "L_at_Amax", "VonBert_K", "CV_young", "CV_old")
  slxpars <- NULL
  if (counter == 100) {
    slxpars <- mapply(gsub, replacement = 1:4,
      MoreArgs = list(pattern = "1", x = "SizeSel_2P_1_Survey"))
    allgrowth <- c(allgrowth, slxpars)
  }
  growthint <- rep(NA, length(allgrowth))
  growthphase <- rep(-1, length(allgrowth))

  # All parameters are fixed at their true OM values
    writeE(allgrowth, growthint, growthphase, my.spp[spp], counter)
  # All parameters are estimated
    writeE(NULL, "NA", "NA", my.spp[spp], counter + 1)
    if (counter == 100) {
      writeE(slxpars, rep(NA, length(slxpars)), rep(-1, length(slxpars)),
        my.spp[spp], counter + 1)
    }
  # All parameters are externally estimated
    growthint[1:5] <- "change_e_vbgf"
    writeE(allgrowth, growthint, growthphase, my.spp[spp], counter + 2)
  # CV's are internally estimated
    writeE(c("L_at_Amin", "L_at_Amax", "VonBert_K", slxpars),
      c(rep("change_e_vbgf", 3), rep(NA, length(slxpars))),
      rep(-1, length(slxpars) + 3), my.spp[spp], counter + 3)
  if (counter == 0){
    # Misspecify M
      for(i in seq_along(mrange[[spp]])) {
        writeE(c("NatM_p_1_Fem"), mrange[[spp]][i], -1, my.spp[spp], 19 + i)
        writeE(c("NatM_p_1_Fem", allgrowth), c(mrange[[spp]][i], growthint),
          c(-1, growthphase), my.spp[spp], 9 + i)
      }}
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
