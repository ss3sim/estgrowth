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
setwd(wd.curr)
setwd(dir.results)
resultfiles <- dir(dir.dropbox, pattern = ".csv", full.names = TRUE)
scalars <- do.call("rbind", lapply(grep("scalar", resultfiles, value = TRUE), 
                   read.csv))
ts <- do.call("rbind", lapply(grep("ts", resultfiles, value = TRUE), read.csv))
# Calculate the relative error for scalars
results_re <- calculate_re(scalars, FALSE)

###############################################################################
###############################################################################
#### Step
#### Name cases
###############################################################################
###############################################################################
# Change E0:5 to the following short names
levels(results_re$E) <- c("fix", "int", "ext", "ext_CV", "ext_LK")
levels(ts$E) <- c("fix", "int", "ext", "ext_CV", "ext_LK")

# Change S0:1 to the following short names
levels(results_re$S) <- c("asymp", "dome")
levels(ts$S) <- c("asymp", "dome")

# Change how A is plotted, with A1 being last because it is the case with 
# no age data
results_re$A <- factor(results_re$A, levels = c("A0", "A2", "A3", "A4", "A5", "A1"))
ts$A <- factor(ts$A, levels = c("A0", "A2", "A3", "A4", "A5", "A1"))

###############################################################################
###############################################################################
#### Step
#### Set up plots and subset data
###############################################################################
###############################################################################
axis.val <- TRUE
axis.rel <- TRUE
my.horiz <- "E"
my.horiz2 <- "D"
my.vert <- "A"
my.vert2 <- "C"
my.x <- "L"
data.plot <- subset(results_re, A %in% c("A10", "A30", "A31") & 
                    L %in% c("L10", "L30", "L31") & D %in% c("D10", "D30") & C %in% c("C10","C20"))
data.plot<-results_re
ts <- subset(ts, A %in% c("A10", "A30", "A31") & 
             L %in% c("L10", "L30", "L31") & D %in% c("D10", "D30") & C %in% c("C10","C20"))

termSSB <- with(subset(ts, year == max(year)), 
                ((SpawnBio_om - SpawnBio_em) / SpawnBio_om))
termSSB <- cbind(termSSB, subset(ts, year == max(year)))

###############################################################################
###############################################################################
#### Step
#### Plots
###############################################################################
###############################################################################
plot_scalar_boxplot(data.plot, x = my.x, y = "CV_old_Fem_GP_1_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2,
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: cv old")
ggsave("cvold.png", dpi = 300)
dev.off()

plot_scalar_boxplot(data.plot, x = my.x, y = "CV_young_Fem_GP_1_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: cv young")
ggsave("cvyoung.png", dpi = 300)
dev.off()

plot_scalar_boxplot(data.plot, x = my.x, y = "L_at_Amin_Fem_GP_1_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: length at A min")
ggsave("latamin.png", dpi = 300)
dev.off()

plot_scalar_boxplot(data.plot, x = my.x, y = "L_at_Amax_Fem_GP_1_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: length at A max")
ggsave("latamax.png", dpi = 300)
dev.off()

plot_scalar_boxplot(data.plot, x = my.x, y = "VonBert_K_Fem_GP_1_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2,
                    horiz2 = my.horiz2, 
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: Vonb K")
ggsave("vonbk.png", dpi = 300)
dev.off()

plot_scalar_boxplot(data.plot, x = my.x, y = "depletion_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: depletion")
ggsave("depletion.png", dpi = 300)
dev.off()

plot_scalar_boxplot(data.plot, x = my.x, y = "SSB_MSY_re", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: SSB at MSY")
ggsave("ssbmsy.png", dpi = 300)
dev.off()

plot_scalar_boxplot(termSSB, x = my.x, y = "termSSB", 
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2, 
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) + 
xlab("Length comps for fishery and survey vs. just fishery") + 
ylab("relative error: terminal SSB")
ggsave("termssb.png", dpi = 300)
dev.off()


scalar_long <- reshape2::melt(subset(data.plot, E == "E2",
  select = c("scenario", "E", "A", "L", "D", "C",
  "replicate", "CV_old_Fem_GP_1_re", "CV_young_Fem_GP_1_re", 
  "L_at_Amin_Fem_GP_1_re", "L_at_Amax_Fem_GP_1_re", "VonBert_K_Fem_GP_1_re")), 
  id.vars = c("scenario", "A", "L", "D", "C", "E", "replicate"))
scalar_long <- plyr::rename(scalar_long, c("value" = "relative_error"))
levels(scalar_long$variable) <- c("CV_old", "CV_young", "L_min", "L_max", "K")
ggplot(subset(scalar_long), aes(D,"relative_error")) +
geom_boxplot(size = 0.2, outlier.size = 1) + 
geom_hline(yintercept = 0, col = "red") + 
geom_hline(aes(yintercept = 0), lty = 2) +
facet_grid(variable ~ A + D) + ylim(-1, 1) + theme_bw() + 
xlab("Length comps for fishery and survey vs. just fishery")
ggsave("compareE2.png", dpi = 300)
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