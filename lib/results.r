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
levels(results_re$S) <- c("asymp", "dome")
results_re$A <- factor(results_re$A, levels = c("A0", "A2", "A3", "A4", "A1"))

levels(ts$E) <- c("fix", "int", "ext", "ext_CV", "ext_LK")
levels(ts$S) <- c("asymp", "dome")
ts$A <- factor(ts$A, levels = c("A0", "A2", "A3", "A4", "A1"))
###############################################################################
###############################################################################
#### Step
#### 
###############################################################################
###############################################################################
axis.val <- TRUE
axis.rel <- TRUE
my.horiz <- "E"
my.horiz2 <- "X"
my.vert <- "A"
my.vert2 <- "S"
my.x <- "L"
data.plot <- subset(results_re, E != "fix")
ts <- subset(ts, E != "fix")

termSSB <- with(subset(ts, year == max(year)), 
                ((SpawnBio_om - SpawnBio_em) / SpawnBio_om))
termSSB <- cbind(termSSB, subset(ts, year == max(year)))

###############################################################################
###############################################################################
#### Step
#### 
###############################################################################
###############################################################################
png("cvold.png")
plot_scalar_boxplot(data.plot, x = my.x, y = "CV_old_Fem_GP_1_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2,
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: cv old")
dev.off()

png("cvyoung.png")
plot_scalar_boxplot(data.plot, x = my.x, y = "CV_young_Fem_GP_1_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: cv young")
dev.off()

png("latamin.png")
plot_scalar_boxplot(data.plot, x = my.x, y = "L_at_Amin_Fem_GP_1_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: length at A min")
dev.off()

png("latamax.png")
plot_scalar_boxplot(data.plot, x = my.x, y = "L_at_Amax_Fem_GP_1_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: length at A max")
dev.off()

png("vonbk.png")
plot_scalar_boxplot(data.plot, x = my.x, y = "VonBert_K_Fem_GP_1_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2,
                    horiz2 = my.horiz2, 
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: Vonb K")
dev.off()

png("depletion.png")
plot_scalar_boxplot(data.plot, x = my.x, y = "depletion_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: depletion")
dev.off()

png("ssbmsy.png")
plot_scalar_boxplot(data.plot, x = my.x, y = "SSB_MSY_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: SSB at MSY")
dev.off()

png("termssb.png")
plot_scalar_boxplot(termSSB, x = my.x, y = "termSSB", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: terminal SSB")
dev.off()

###############################################################################
###############################################################################
#### Step
#### Plot data availability
###############################################################################
###############################################################################
png("samples.png")
fulldata <- lapply(do.call("rbind", 
                    strsplit(readLines("../casefiles/lcomp0-col.txt")[1:3], 
                             ";"))[, 2],
              function(x) eval(parse(text = x)))
lessdata <- lapply(do.call("rbind", 
                    strsplit(readLines("../casefiles/lcomp2-col.txt")[1:3], 
                             ";"))[, 2],
              function(x) eval(parse(text = x)))
mindata <- lapply(do.call("rbind", 
                    strsplit(readLines("../casefiles/lcomp4-col.txt")[1:3], 
                             ";"))[, 2],
              function(x) eval(parse(text = x)))
plot(1:100, 1:100, ylim = c(5, 42), las = 1, yaxt = "n",
     ylab = "Length and age comp sample size", xlab = "year", type = "n")
axis(2, at = c(10, 40), las = 1)
points(fulldata[[3]][[1]], fulldata[[2]][[1]] + 1, pch = 1, cex = 1)
points(fulldata[[3]][[1]], fulldata[[2]][[1]] - 2, pch = 1, cex = 1)
points(fulldata[[3]][[2]], fulldata[[2]][[2]] - 0, pch = 19, cex = 1)
points(lessdata[[3]][[1]], lessdata[[2]][[1]] - 1, pch = 1, cex = 1)
points(lessdata[[3]][[1]], lessdata[[2]][[1]] + 3, pch = 1, cex = 1)
points(lessdata[[3]][[2]], lessdata[[2]][[2]] + 2, pch = 19, cex = 1)
points(mindata[[3]][[1]], mindata[[2]][[1]] - 4, pch = 1, cex = 1)
legend(80, 25, pch = c(1,19), legend = c("Fishery", "Survey"), 
       bty = "n", cex = 1.2, ncol = 1)
text(x = 10, y = 41, labels = "L0, A0")
text(x = 10, y = 38, labels = "L1, A2")
text(x = 10, y = 13, labels = "L2, A3")
text(x = 10, y = 9, labels = "L3, A4")
text(x = 10, y = 6, labels = "L4, A5")
dev.off()

###############################################################################
###############################################################################
#### Step
#### Upon exit return to the original working directory
###############################################################################
###############################################################################
setwd(wd.curr)