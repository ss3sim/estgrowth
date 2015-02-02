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
start <- 1
end   <- 100
start.survey  <- start + 75
start.fishery <- start + 25
# Number of years the fishery ramps up before starting two way trip
years.rup <- 41
all.surv <- seq(start.survey, end, by = 2)
all.fish <- c(seq(start.fishery, start.fishery + 20, by = 10),
              seq(start.fishery + 30, end, by = 4))
# Amount to cut data by (e.g., if 2 take every other point)
reducer <- 2
# Number of years randomly picked for mlacomps
nmlayears <- 2

# lcomp and age comp sample sizes
high <- 40
# Currently not using the lower sample size as Ono et al. (2014)
# showed sample size really did not matter
low <- 10

# Estimate CVs or not
estCVs <- TRUE

# A list the length of number of spp where each entry is the amount to
# add or subtract from the double normal currently programmed as asymptotic
dome <- list(c(51.5 - 50.8, -4 - -3, 5.2 - 5.1, 8 - 15, 0, 0))
# amount of random noise in selectivity for each spp - pertains to parameter 1
tvs <- c(0.1)

# Sequence true natural morality values for each species by 10 - 190%
truem <- c(0.2, 0.2, 0.5)
mrange <- lapply(truem, function(x) x * seq(.1, 1.90, by = 0.1))
###############################################################################
###############################################################################
#### Step
#### Functions to create casefiles
###############################################################################
###############################################################################
# F == Fishing mortality
writeF <- function(start, end, fvals, species, case) {
  sink(paste0("F", case, "-", species, ".txt"))
  cat("years; c(", paste(start:end, collapse = ", "), ")\n",
      "years_alter; c(", paste(start:end, collapse = ", "), ")\n",
      "fvals; c(", paste(fvals, collapse = ", "), ")\n", sep = "")
  sink()
}

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

# A & L sample age and length comp data
writeL <- function(Nsamp.fish, Nsamp.survey, years.fish, years.survey,
                   type, case, spp) {
  fish <- ifelse(is.null(years.fish), FALSE, TRUE)
  survey <- ifelse(is.null(years.survey), FALSE, TRUE)
  years <- "NULL"
  Nsamp <- "NULL"
  fleets <- "NULL"
  cparval <- "NULL"
  if (all(fish, survey)) {
      years <- paste0("list(c(", paste(years.fish, collapse = ","),
                      "), c(", paste(years.survey, collapse = ","), "))")
      Nsamp <- paste0("list(c(", paste(Nsamp.fish, collapse = ","),
                      "), c(", paste(Nsamp.survey, collapse = ","), "))")
      fleets  <- "c(1, 2)"
      cparval <- "c(2, 1)"
  } else {
    if (fish) {
      years <- paste0("list(c(", paste(years.fish, collapse = ","), "))")
      Nsamp <- paste0("list(c(", paste(Nsamp.fish, collapse = ","), "))")
      fleets  <- "c(1)"
      cparval <- "c(2)"
    }
    if (survey) {
      years <- paste0("list(c(", paste(years.survey, collapse = ","), "))")
      Nsamp <- paste0("list(c(", paste(Nsamp.survey, collapse = ","), "))")
      fleets <- "c(2)"
      cparval <- "c(1)"
    }
  }
  l <- c(paste0("fleets; ", fleets),
         paste("Nsamp;", Nsamp),
         paste("years;", years),
         paste("cpar;", cparval))
  writeLines(l, paste0(type, case, "-", spp, ".txt"))
}

# D sample mlacomp data
writeD <- function(fleets, years, Nsamp, species, case,
                   mean_outfile = "NULL") {
  if (is.null(fleets)) {
    a.fleet <- "NULL"
  } else {
    a.fleet <- paste("c(", paste(fleets, collapse = ", "), ")")
  }
  a <- c(paste("fleets;", a.fleet),
         paste("years;", years),
         paste("Nsamp;", Nsamp),
         paste("mean_outfile;", mean_outfile))
  writeLines(a, paste0("mlacomp", case, "-", species, ".txt"))
}

# Change selectivity to be dome or time-varying in OM
writeS <- function(vals, species, case,
                   type = c("random", "deviates")) {
  type <- match.arg(type, several.ok = FALSE)
  parnames <- paste0("SizeSel_1P_", 1:6, "_Fishery")
  beg <- paste("function_type; change_tv")
  sec <- "param;"
  mid <- "dev; rnorm(n = 100, mean = 0, sd = "
  end <- ")"
  let <- toupper(rev(letters)[1:6])

  lapply(seq_along(parnames), function(x) {
    if(type == "deviates") {
      info <- c(beg, paste(sec, parnames[x]),
                paste0("dev; rep(", vals[x], ", 100)"))
    } else {
      info <- c(beg, paste(sec, parnames[x]), paste0(mid, vals[x], end))
    }
    writeLines(info, paste0(let[x], case, "-", species, ".txt"))
  })
}

# Add time-varying random normal deviates to M
writeM <- function(deviates, species, case) {
  beg <- paste("function_type; change_tv")
  sec <- "param;"
  mid <- "dev; "

  info <- c(beg, paste(sec, "NatM_p_1_Fem_GP_1"),
            paste0(mid, paste0(deviates)))
  writeLines(info, paste0("M", case, "-", species, ".txt"))
}

  # End of functions for creating case files

###############################################################################
## Step
## Sequence along all species listed in my.spp
###############################################################################
for (spp in seq_along(my.spp)) {

###############################################################################
## Step
## Standard case files
## These case files do not change per scenario
## Retro and index
###############################################################################
writeLines("retro_yr; 0", paste0("R0-", my.spp[spp], ".txt"))

# Years of survey index of abundance
writeLines(c("fleets; 2", paste0("years; list(c(",
             paste(all.surv, collapse = ","), "))"), "sds_obs; list(0.2)"),
             paste0("index0-", my.spp[spp], ".txt"))

###############################################################################
## Step
## Fishing patterns: F
###############################################################################
#TODO: Find FMSY for each species
fmsy <- c(0.07, 0.07, 0.175)
fmsy.u90 <- c(0.175, 0.175, 0.175)
# Calculate the burnin period
years.burnin <- start.fishery - start - 1
# Determine the number of years left for the rampdown in two-way trip
years.rdown  <- length(start:end) - (years.burnin + years.rup)

# Constant F for 75 years
writeF(start = start, end = end,
       fvals = c(rep(0, years.burnin), rep(fmsy[spp], years.rup + years.rdown)),
       species = my.spp[spp], case = 0)
# Burn in of 0 for 25 years, up to 0.9*Fmsy (right limb) for 40 years,
# down to 0.9*Fmsy (left limb value) for 35years
writeF(start = start, end = end,
       fvals = c(rep(0, years.burnin),
                 seq(0, fmsy.u90[spp], length.out = years.rup),
                 seq(fmsy.u90[spp], fmsy[spp], length.out = years.rdown)),
       species = my.spp[spp], case = 1)
# Burn in of 0 for 25 years, up to 0.9*Fmsy (left limb) for 75 years,
writeF(start = start, end = end,
       fvals = c(rep(0, years.burnin),
                 seq(0, fmsy.u90[spp], length.out = years.rup + years.rdown)),
       species = my.spp[spp], case = 1)

###############################################################################
###############################################################################
#### Step
#### change_e: case "E"
###############################################################################
###############################################################################
# Change to the below code if we want to also estimate the CV parameters
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

# Misspecify M
  for(i in seq_along(mrange)) {
    writeE(c("NatM_p_1_Fem"), mrange[[spp][i], -1, my.spp[spp], 100 + i)
    writeE(c("NatM_p_1_Fem", allgrowth),
           c(mrange[[spp][i], rep("change_e_vbgf", length(allgrowth))),
           c(-1, growthphase), my.spp[spp], 200 + i)
  }

###############################################################################
###############################################################################
#### Step
#### change_lcomp: case "L" and change_agecomp: case "A"
#### Both sets of casefiles are created with writeL
#### change "type" to create length or age casefiles
#### if fishery then cpar = 2, if survey cpar = 1
###############################################################################
###############################################################################
less <- sapply(c("surv", "fish"), function(x) {
                realword <- paste("all", x, sep = ".")
                vals <- eval(parse(text = realword))
                vals[seq.int(1L, length(vals), reducer)]
               })
counter <- 0
# Length data for all years of fishery and survey
writeL(Nsamp.fish = rep(high, length(all.fish)),
       Nsamp.survey = rep(high, length(all.surv)),
       years.fish = all.fish, years.survey = all.surv,
       type = "lcomp", case = counter, spp = my.spp[spp])
# Length data from just a fishery for all years
writeL(Nsamp.fish = rep(high, length(all.fish)), Nsamp.survey = NULL,
       years.fish = all.fish, years.survey = NULL,
       type = "lcomp", case = counter + 1, spp = my.spp[spp])
# Length data for reduced years from survey and all fishery years
writeL(Nsamp.fish = rep(high, length(all.fish)),
       Nsamp.survey = rep(high, length(less$surv)),
       years.fish = all.fish, years.survey = less$surv,
       type = "lcomp", case = counter + 2, spp = my.spp[spp])

counter <- 0
# Age data for all years of fishery and survey
writeL(Nsamp.fish = rep(high, length(all.fish)),
       Nsamp.survey = rep(high, length(all.surv)),
       years.fish = all.fish, years.survey = all.surv,
       type = "agecomp", case = counter, spp = my.spp[spp])
# Age data from just a fishery for all years
writeL(Nsamp.fish = rep(high, length(all.fish)), Nsamp.survey = NULL,
       years.fish = all.fish, years.survey = NULL,
       type = "agecomp", case = counter + 1, spp = my.spp[spp])
# Age data for reduced years from survey and all fishery years
writeL(Nsamp.fish = rep(high, length(all.fish)),
       Nsamp.survey = rep(high, length(less$surv)),
       years.fish = all.fish, years.survey = less$surv,
       type = "agecomp", case = counter + 2, spp = my.spp[spp])
# No age data
writeL(Nsamp.fish = NULL,
       Nsamp.survey = NULL,
       years.fish = NULL, years.survey = NULL,
       type = "agecomp", case = counter + 3, spp = my.spp[spp])

###############################################################################
###############################################################################
#### Step
#### sample_mlacomp data: D
###############################################################################
###############################################################################
years.txt <- lapply(list(as = all.surv, af = all.fish, ls = less$surv, lf = less$fish),
                    function(x) {
                      paste0("c(", paste(x, collapse = ", "), ")")
                    })
counter <- 0

# No mla comp data
writeD(fleets = NULL, years = "NULL", Nsamp = "NULL",
       my.spp[spp], case = counter)
# Create mla comp data for fishery and then for survey
# for the number of random years set in nmlayears
for(q in c("vbgf_remove")) {
  writeD(fleets = 1,
        years = paste("list(sample(", years.txt$ls, ", ", nmlayears, ", replace = FALSE))"),
       Nsamp = "list(50)", my.spp[spp], case = counter + 1, mean_outfile = q)
  counter <- counter + 1
  writeD(fleets = 2,
        years = paste("list(sample(", years.txt$as, ", ", nmlayears, ", replace = FALSE))"),
       Nsamp = "list(50)", my.spp[spp], case = counter + 19, mean_outfile = q)
  writeD(fleets = 2,
        years = paste("list(sample(", years.txt$ls, ", ", nmlayears, ", replace = FALSE))"),
       Nsamp = "list(50)", my.spp[spp], case = counter + 20, mean_outfile = q)
  counter <- counter + 2
}

###############################################################################
###############################################################################
#### Step
#### Time varying selectivity case files
###############################################################################
###############################################################################
writeS(vals = rep(0, 6), my.spp[spp], case = 0, type = "deviates")
# TODO: make the change to dome-shaped selectivity spp specific
writeS(vals = c(tvs[spp], rep(0, 5)), my.spp[spp], case = 2, type = "random")
writeS(vals = dome[[spp]],
       my.spp[spp], case = 1, type = "deviates")

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