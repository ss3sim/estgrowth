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
## Step 
## Set working directories 
###############################################################################

wd.curr <- getwd()
setwd(dir.cases)
spp.case <- "col"
start.survey <- 76
start.fishery <- 26
start <- 1
end <- 100

###############################################################################
###############################################################################
#### Step
#### Functions to create casefiles
###############################################################################
###############################################################################
f.info <- c(paste0("years; c(", paste(
        #All years in the simulation
          start:end, 
        collapse = ","), ")"),
        paste0("years_alter; c(", paste(
        #Years to alter F
          start:end, 
        collapse = ","), ")"))

writeE <- function(growthname, growthint, growthphase, species, case) {
    sink(paste0("E", case, "-", species, ".txt"))
      cat("natM_type; 1Parm \nnatM_n_breakpoints; NULL \n",
           "natM_lorenzen; NULL \nnatM_val; c(NA, NA) \n", sep = "")
      if(is.null(growthname)) {cat("par_name; NULL \n")} else{
        cat("par_name; c(\"", paste0(growthname, collapse = "\", \""), "\") \n", sep = "")
      }
      cat(paste("par_int;", growthint), "\n")
      cat(paste("par_phase;", growthphase), "\n")
      cat("forecast_num; 0 \nrun_change_e_full; TRUE \n")
  sink()
}

writeL <- function(Nsamp.fish, Nsamp.survey, years.fish, years.survey,
                   type, case, spp) {
  fish <- ifelse(is.null(years.fish), FALSE, TRUE)
  survey <- ifelse(is.null(years.survey), FALSE, TRUE)
  years <- "NULL"
  Nsamp <- "NULL"
  fleets <- "NULL"
  if(all(fish, survey)) {
      years <- paste0("list(c(", paste(years.fish, collapse = ","),
                      "), c(", paste(years.survey, collapse = ","), "))")
  } else {
    if(fish) {
    years <- paste0("list(c(", paste(years.fish, collapse = ","),
                      "))")      
    }
    if(survey) {
      years <- paste0("list(c(", paste(years.survey, collapse = ","),
                      "))")
    }
  }
  if(all(fish, survey)) {
    Nsamp <- paste0("list(c(", paste(Nsamp.fish, collapse = ","),
                    "), c(", paste(Nsamp.survey, collapse = ","), "))")
  } else {
    if(fish) {
    Nsamp <- paste0("list(c(", paste(Nsamp.fish, collapse = ","),
                    "))")
    }
    if(survey) {
    Nsamp <- paste0("list(c(", paste(Nsamp.survey, collapse = ","),
                    "))")
    }
  }
  if(all(fish, survey)) {
    fleets <- "c(1, 2)"
  } else {
    if(fish) fleets <- "c(1)"
    if(survey) fleets <- "c(2)"
  }

  l <- c(paste0("fleets; ", fleets),
         paste("Nsamp;", Nsamp),
         paste("years;", years),
         "cpar; 1",
         "write_file; TRUE")
  writeLines(l, paste0(type, case, "-", spp, ".txt"))
}

writeX <- function(fleets, years, Nsamp, species, case) {
  a <- c(paste("fleets;", fleets),
         paste("years;", years),
         paste("Nsamp;", Nsamp),
         "write_file; TRUE")
  writeLines(a, paste0("mlacomp", case, "-", species, ".txt"))
}

writeS <- function(vals, species, case) {
  parnames <- paste0("SizeSel_1P_", 1:6, "_Fishery")
  beg <- paste("function_type; change_tv")
  sec <- "param;"
  mid <- "dev; rep("
  end <- ", 100)"
  let <- toupper(letters[10:15])
  
  lapply(seq_along(parnames), function(x) {
    info <- c(beg, paste(sec, parnames[x]), paste0(mid, vals[x], end))
    writeLines(info, paste0(let[x], case, "-", species, ".txt"))
  })
}

###############################################################################
## Step 
## Standard case files
## These case files do not change per scenario
## Retro and index
###############################################################################
for(spp in seq_along(spp.case)) {
writeLines("retro_yr; 0", paste0("R0-", spp.case[spp], ".txt"))

index0 <- c("fleets; 2", 
            paste0("years; list(c(", paste(
            #Years of survey index of abundance
              seq(start.survey, end, by = 2), 
            collapse = ","),
                   "))"),
            "sds_obs; list(0.2)")
writeLines(index0, paste0("index0-", spp.case[spp], ".txt"))

###############################################################################
## Step 
## Fishing patterns: F
###############################################################################
#change_f: case "F"
## Constant F for 75 years
f0 <- c(f.info, paste0("fvals; c(", paste(
        #F vals for each year
          c(rep(0, start.fishery - start), rep(0.07, end - start.fishery + 1)
        ), collapse = ","), ")"))
writeLines(f0, paste0("F0-", spp.case[spp], ".txt"))

## Burn in of 0 for 25 years, up to 0.9*Fmsy (right limb) for 40 years,
## down to 0.9*Fmsy (left limb value) for 35years
f1 <- c(f.info, paste0("fvals; c(", paste(
        #F vals for each year
          c(rep(0, start.fishery - start - 1), seq(0, 0.175, length.out = 41),
            seq(0.175, 0.07, length.out = 35)), 
        collapse = ","),")"))
writeLines(f1, paste0("F1-", spp.case[spp], ".txt"))

## Burn in of 0 for 25 years, up to 0.9*Fmsy (left limb) for 75 years,
f2 <- c(f.info, paste0("fvals; c(", paste(
        #F vals for each year
          c(rep(0, start.fishery - start - 1), seq(0, 0.175, length.out = 76)),
        collapse = ","),")"))
writeLines(f2, paste0("F2-", spp.case[spp], ".txt"))

###############################################################################
###############################################################################
#### Step
#### change_e: case "E"
###############################################################################
###############################################################################
allgrowth <- c("L_at_Amin", "L_at_Amax", "VonBert_K", "CV_young", "CV_old")
growthint <- rep(NA, length(allgrowth))
growthphase <- rep(-1, length(allgrowth))

writeE(allgrowth, "rep(NA, 5)", "rep(-1, 5)", spp.case[spp], 0)
writeE(NULL, "NA", "NA", spp.case[spp], 1)
writeE(allgrowth, "rep(\"change_e_vbgf\", 5)", "rep(-1, 5)", spp.case[spp], 2)
writeE(allgrowth, "c(NA, NA, NA, rep(\"change_e_vbgf\", 2))", "c(2, 2, 2, rep(-1, 2))", spp.case[spp], 3)
writeE(allgrowth, "c(rep(\"change_e_vbgf\", 3), NA, NA)", "c(rep(-1, 3), 2, 2)", spp.case[spp], 4)

###############################################################################
###############################################################################
#### Step
#### change_lcomp: case "L" and change_agecomp: case "A"
#### Both sets of casefiles are created with writeL
#### change "type" to create length or age casefiles
###############################################################################
###############################################################################
high <- 40
low <- 10
all.fish <- c(seq(start.fishery, start.fishery + 10, by = 10), 
              seq(start.fishery + 20, start.fishery + 45, by = 5),
              seq(start.fishery + 46, end))
all.surv <- seq(start.survey, end, by = 2)

less.fish <- all.fish[seq(1, length(all.fish), by = 4)]
less.surv <- all.surv[seq(1, length(all.surv), by = 4)]

writeL(Nsamp.fish = rep(high, length(all.fish)), Nsamp.survey = rep(high, length(all.surv)), 
       years.fish = all.fish, years.survey = all.surv, 
       type = "lcomp", case = 0, spp = spp.case[spp])
writeL(Nsamp.fish = rep(high, length(all.fish)), Nsamp.survey = NULL, 
       years.fish = all.fish, years.survey = NULL, 
       type = "lcomp", case = 1, spp = spp.case[spp])
writeL(Nsamp.fish = rep(low, length(all.fish)), Nsamp.survey = rep(low, length(all.surv)), 
       years.fish = all.fish, years.survey = all.surv, 
       type = "lcomp", case = 2, spp = spp.case[spp])
writeL(Nsamp.fish = rep(low, length(all.fish)), Nsamp.survey = NULL, 
       years.fish = all.fish, years.survey = NULL, 
       type = "lcomp", case = 3, spp = spp.case[spp])
writeL(Nsamp.fish = rep(low, length(less.fish)), Nsamp.survey = NULL,
       years.fish = less.fish, years.survey = NULL,
       type = "lcomp", case = 4, spp = spp.case[spp])

writeL(Nsamp.fish = rep(high, length(all.fish)), Nsamp.survey = rep(high, length(all.surv)), 
       years.fish = all.fish, years.survey = all.surv, 
       type = "agecomp", case = 0, spp = spp.case[spp])
writeL(Nsamp.fish = NULL, Nsamp.survey = NULL, years.fish = NULL, years.survey = NULL, 
       type = "agecomp", case = 1, spp = spp.case[spp])
writeL(Nsamp.fish = rep(high, length(all.fish)), Nsamp.survey = NULL, 
       years.fish = all.fish, years.survey = NULL, 
       type = "agecomp", case = 2, spp = spp.case[spp])
writeL(Nsamp.fish = rep(low, length(all.fish)), Nsamp.survey = rep(low, length(all.surv)), 
       years.fish = all.fish, years.survey = all.surv, 
       type = "agecomp", case = 3, spp = spp.case[spp])
writeL(Nsamp.fish = rep(low, length(all.fish)), Nsamp.survey = NULL, 
       years.fish = all.fish, years.survey = NULL, 
       type = "agecomp", case = 4, spp = spp.case[spp])
writeL(Nsamp.fish = rep(low, length(less.fish)), Nsamp.survey = NULL,
       years.fish = less.fish, years.survey = NULL,
       type = "agecomp", case = 5, spp = spp.case[spp])

###############################################################################
###############################################################################
#### Step
#### sample_mlacomp data: X
###############################################################################
###############################################################################
writeX(fleets = "NULL", years = "NULL", Nsamp = "NULL", spp.case[spp], case = 0)
writeX(fleets = "c(1)", years = "list(c(26))", 
       Nsamp = "list(50)", spp.case[spp], case = 1)
writeX(fleets = "c(2)", years = paste0("list(",start.survey,")"), 
       Nsamp = "list(50)", spp.case[spp], case = 2)

###############################################################################
###############################################################################
#### Step
#### Time varying selectivity case files
###############################################################################
###############################################################################
writeS(vals = rep(0, 6), spp.case[spp], case = 0)
writeS(vals = c(51.5 - 50.8, -4 - -3, 5.2 - 5.1, 8 - 15, 0, 0), 
       spp.case[spp], case = 1)



}
###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################
setwd(wd.curr)