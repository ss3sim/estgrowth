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
#### Get and manipulate result files from the shared dropbox folder
###############################################################################
###############################################################################
if(!"ggplot2" %in% rownames(installed.packages())) {
    install.packages("ggplot2")
}
library(ggplot2)

wd.curr <- getwd()
setwd(dir.results)
resultfiles <- dir(dir.dropbox, pattern = ".csv", full.names = TRUE)
scalars <- do.call("rbind", lapply(grep("scalar", resultfiles, value = TRUE), 
                   read.csv))
ts <- do.call("rbind", lapply(grep("ts", resultfiles, value = TRUE), read.csv))
results_re <- calculate_re(scalars, FALSE)


###############################################################################
###############################################################################
#### Step
#### Name cases
###############################################################################
###############################################################################
levels(results_re$E) <- c("fix", "int", "ext", "ext_CV", "ext_LK")
levels(results_re$species) <- c("dome", "asymp")

###############################################################################
###############################################################################
#### Step
#### 
###############################################################################
###############################################################################
axis.val <- TRUE

png("cvold.png")
plot_scalar_boxplot(subset(results_re, E != "fix"), x = "L", y = "CV_old_Fem_GP_1_re", 
                    vert="A", horiz="E", vert2 = "species", 
                    rel = FALSE, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: cv old")
dev.off()

png("cvyoung.png")
plot_scalar_boxplot(subset(results_re, E != "fix"), x = "L", y = "CV_young_Fem_GP_1_re", 
                    vert="A", horiz="E", vert2 = "species", 
                    rel = FALSE, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: cv young")
dev.off()

png("latamin.png")
plot_scalar_boxplot(subset(results_re, E != "fix"), x = "L", y = "L_at_Amin_Fem_GP_1_re", 
                    vert="A", horiz="E", vert2 = "species", 
                    rel = FALSE, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: length at A min")
dev.off()

png("latamax.png")
plot_scalar_boxplot(subset(results_re, E != "fix"), x = "L", y = "L_at_Amax_Fem_GP_1_re", 
                    vert="A", horiz="E", vert2 = "species", 
                    rel = FALSE, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: length at A max")
dev.off()

png("vonbk.png")
plot_scalar_boxplot(subset(results_re, E != "fix"), x = "L", y = "VonBert_K_Fem_GP_1_re", 
                    vert="A", horiz="E", vert2 = "species", 
                    rel = FALSE, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: Vonb K")
dev.off()

###############################################################################
###############################################################################
#### Step
#### Plot data availability
###############################################################################
###############################################################################

fulldata <- lapply(do.call("rbind", 
                    strsplit(readLines("../casefiles/lcomp0-cod.txt")[1:3], 
                             ";"))[, 2],
              function(x) eval(parse(text = x)))
plot(1913:2012, 1913:2012, ylim = c(0, 110), xaxt = "n", las = 1, 
     ylab = "n samples", xlab = "years")
axis(1, seq(1912, 2012, by = 5), labels = seq(0, 100, by = 5))
points(fulldata[[3]][[1]], fulldata[[2]][[1]], pch = 1, cex = 3)
points(fulldata[[3]][[2]], fulldata[[2]][[2]], pch = 18, cex = 3)
legend("topleft", pch = c(1,18), legend = c("Fishery", "Survey"), 
       bty = "n", cex = 4)

###############################################################################
###############################################################################
#### Step
#### Upon exit return to the original working directory
###############################################################################
###############################################################################
setwd(wd.curr)