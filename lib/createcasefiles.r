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
## Step 
## Standard case files
###############################################################################
for(spp in seq_along(spp.case)) {
r0 <- "retro_yr; 0"
writeLines(r0, paste0("R0-", spp.case[spp], ".txt"))

index0 <- c("fleets; 2", 
            paste0("years; list(c(", paste(seq(start.survey, end, by = 2), collapse = ","), "))"),
            paste0("sds_obs; list(0.2)"))
writeLines(index0, paste0("index0-", spp.case[spp], ".txt"))

f0 <- c(paste0("years; c(", paste(start:end, collapse = ","), ")"),
        paste0("years_alter; c(", paste(start:end, collapse = ","), ")"),
        paste0("fvals; c(", paste(c(rep(0, start.fishery - start), 
                          rep(0.08, end - start.fishery + 1)), collapse = ","), ")"))
writeLines(f0, paste0("F0-", spp.case[spp], ".txt"))
f1 <- c(paste0("years; c(", paste(start:end, collapse = ","), ")"),
        paste0("years_alter; c(", paste(start:end, collapse = ","), ")"),
        paste0("fvals; c(", paste(c(rep(0, start.fishery - start), 
                                  seq(0, 0.16, length.out = 60),
                                  seq(0.16, 0.08, length.out = 15)), 
                                  collapse = ","),")"))
writeLines(f1, paste0("F1-", spp.case[spp], ".txt"))
###############################################################################
## Step 
## Case files that change
###############################################################################

#change_e: case "E"
allgrowth <- c("L_at_Amin", "L_at_Amax", "VonBert_K", "CV_young", "CV_old")
growthint <- rep(NA, length(allgrowth))
growthphase <- rep(-1, length(allgrowth))

writeE <- function(growthname, growthint, growthphase, case) {
    sink(paste0("E", case, "-", spp.case[spp], ".txt"))
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

writeE(allgrowth, "rep(NA, 5)", "rep(-1, 5)", 0)
writeE(NULL, "NA", "NA", 1)
writeE(allgrowth, "rep(\"change_e_vbgf\", 5)", "rep(-1, 5)", 2)
writeE(allgrowth, "c(NA, NA, NA, rep(\"change_e_vbgf\", 2))", "c(2, 2, 2, rep(-1, 2))", 3)
writeE(allgrowth, "c(rep(\"change_e_vbgf\", 3), NA, NA)", "c(rep(-1, 3), 2, 2)", 4)

#change_bin: case "bin"
writeB <- function(bin_vector, type, case) {
	b <- c(paste("bin_vector;", bin_vector),
	       paste("type;", type))
	if(is.null(bin_vector)) {b <- b[2]}
	writeLines(b, paste0("bin", case, "-", spp.case[spp], ".txt"))
}

writeB("list(\"age\" = 0:15)", "c(\"age\")", 0)
writeB(NULL, "c(\"cal\")", 1)
writeB("list(\"age\" = 0:15)", "c(\"age\", \"mla\")", 2)
writeB(NULL, "c(\"cal\", \"mla\")", 3)

#change_lcomp: case "L"
#change_agecomp: case "A"

bothfleets <- "c(1, 2)"
justfish <- "c(1)"

allsamples <-  "list(c(20, 20, seq(40, 80, 10), rep(100, 30)), rep(100, 13))"
fishsamples <- "list(c(20, 20, seq(40, 80, 10), rep(100, 30)))"

allsamples.4 <-  "list(c(5, 5, seq(10, 24, 3), rep(25, 30)), rep(25, 13))"
fishsamples.4 <- "list(c(5, 5, seq(10, 24, 3), rep(25, 30)))"

allyears <- paste0("list(c(seq(", start.fishery, ", ", start.fishery + 10, ", by = 10), seq(", 
                    start.fishery + 20, ", ", start.fishery + 45, ", by = 5), seq(", 
                    start.fishery + 46, ", ", end, ")), seq(", start.survey,",", 
                    end,", by = 2))")
fishyears <- paste0("list(c(seq(", start.fishery, ", ", start.fishery + 10, ", by = 10), seq(", 
                    start.fishery + 20, ", ", start.fishery + 45, ", by = 5), seq(", 
                    start.fishery + 46, ", ", end, ")))")

writeL <- function(fleets, Nsamp, years, case) {
	l <- c(paste("fleets;", fleets),
	       paste("Nsamp;", Nsamp),
	       paste("years;", years),
	       "cpar; 1",
	       "lengthbin_vector; NULL",
	       "write_file; TRUE")
	writeLines(l, paste0("lcomp", case, "-", spp.case[spp], ".txt"))
}
writeA <- function(fleets, Nsamp, years, case) {
	a <- c(paste("fleets;", fleets),
	       paste("Nsamp;", Nsamp),
	       paste("years;", years),
	       "cpar; 1",
	       "agebin_vector; NULL",
	       "write_file; TRUE")
	writeLines(a, paste0("agecomp", case, "-", spp.case[spp], ".txt"))
}

writeL(fleets = bothfleets, Nsamp = allsamples, years = allyears, case = 0)
writeL(fleets = justfish, Nsamp = fishsamples, years = fishyears, case = 1)
writeA(fleets = bothfleets, Nsamp = allsamples, years = allyears, case = 0)
writeA(fleets = "NULL", Nsamp = "NULL", years = "NULL", case = 1)
writeA(fleets = justfish, Nsamp = fishsamples, years = fishyears, case = 2)

writeL(fleets = bothfleets, Nsamp = allsamples.4, years = allyears, case = 2)
writeL(fleets = justfish, Nsamp = fishsamples.4, years = fishyears, case = 3)
writeA(fleets = bothfleets, Nsamp = allsamples.4, years = allyears, case = 3)
writeA(fleets = justfish, Nsamp = fishsamples.4, years = fishyears, case = 4)
}

setwd(wd.curr)