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
results <- lapply(resultfiles, read.csv)

setwd(dir.results)

results_re <- calculate_re(results[[1]], FALSE)

plot_scalar_boxplot(results_re, x = "L", y = "SizeSel_1P_1_Fishery_re", 
                    vert="A", horiz="E", rel=FALSE, axes.free=FALSE) + 
xlab("# of length data") + ylab("relative error PARAMETER")