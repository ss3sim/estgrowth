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
#### 
###############################################################################
###############################################################################

wd.curr <- getwd()

library(ggplot2)
resultfiles <- dir("c:/users/kelli/dropbox/estgrowth", 
                   pattern = ".csv", full.names = TRUE)

scalars <- do.call("rbind", lapply(grep("scalar", resultfiles, value = TRUE), read.csv))
ts <- do.call("rbind", lapply(grep("ts", resultfiles, value = TRUE), read.csv))

setwd(dir.results)

results_re <- calculate_re(scalars, FALSE)

plot_scalar_boxplot(results_re, x = "L", y = "L_at_Amin_Fem_GP_1_re", 
                    vert="A", horiz="E", rel=FALSE, axes.free=FALSE) + 
xlab("Length comps for fishery and survey vs. just fishery") + ylab("relative error: Length at A min")

names(results_re)

hist(scalars$depletion_em)

any(is.na(scalars$depletion_em))