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
#### Input that could change according to users
###############################################################################
###############################################################################
my.horiz <- "E"
my.horiz2 <- "C"
my.vert <- "A"
my.vert2 <- "species"
my.x <- "L"

conv.crit <- 0.01

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
library(ss3sim)

# Find auxiliary code with plotting functions
if (file.exists("lib", "plot_functions.R")) {
  source(file.path("lib", "plot_functions.R"))
} else {
  message("Choose file that contains the plot functions.")
  source(file.choose(new = FALSE))
}

# Set up folder structure
if (!exists("dir.dropbox")) {
  dir.dropbox <- choose.dir(caption = "Select the dir containing csv's")
}
if (!exists("dir.results")) {
  dir.results <- choose.dir(caption = "Select plotting directory")
}

# Read in the results
wd.curr <- getwd()
setwd(dir.results)
resultfiles <- list.files(dir.dropbox, pattern = ".csv", full.names = TRUE)
scalars <- read.csv(grep("scalar", resultfiles, value = TRUE), header = TRUE)
ts <- read.csv(grep("ts", resultfiles, value = TRUE), header = TRUE)

# Calculate the relative error for scalars
scalars <- calculate_re(scalars, TRUE)
ts <- calculate_re(ts, TRUE)
results_re <- calculate_re(scalars, FALSE)

###############################################################################
###############################################################################
#### Step
#### Name cases
###############################################################################
###############################################################################
scalars <- change_levels(data = scalars, group = "E",
  old = c("E0", "E1", "E2", "E3"), new = c("fix", "int", "ext", "intCV"))
results_re <- change_levels(data = results_re, group = "E",
  old = c("E0", "E1", "E2", "E3"), new = c("fix", "int", "ext", "intCV"))
ts <- change_levels(data = ts, group = "E",
  old = c("E0", "E1", "E2", "E3"), new = c("fix", "int", "ext", "intCV"))

scalars <- change_levels(data = scalars, group = "A",
  old = c("A0", "A10", "A30"), new = c("noage", "fishage", "bothage"))
results_re <- change_levels(data = results_re, group = "A",
  old = c("A0", "A10", "A30"), new = c("noage", "fishage", "bothage"))
ts <- change_levels(data = ts, group = "A",
  old = c("A0", "A10", "A30"), new = c("noage", "fishage", "bothage"))

scalars <- change_levels(data = scalars, group = "L",
  old = c("L10", "L30"), new = c("fishlen", "bothlen"))
results_re <- change_levels(data = results_re, group = "L",
  old = c("L10", "L30"), new = c("fishlen", "bothlen"))
ts <- change_levels(data = ts, group = "L",
  old = c("L10", "L30"), new = c("fishlen", "bothlen"))

###############################################################################
###############################################################################
#### Step
#### Set up plots and subset data
###############################################################################
###############################################################################
axis.val <- TRUE
axis.rel <- TRUE

#subset for max_grad
maxgrad <- aggregate(max_grad ~ L + A + E + C + F + species, data = scalars,
  function(x) c("median" = median(x), "num" = sum(x > conv.crit)))
# Write to file, those with > 0 above 0.01
write.csv(maxgrad[maxgrad$max_grad[,2] > 0, ], "maxgrad.csv")
data.plot <- subset(results_re, max_grad < conv.crit &
  E %in% c("ext", "int", "intCV"))
ts.plot <- subset(ts, ID %in% data.plot$ID & E %in% c("ext", "int", "intCV"))

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

plot_scalar_boxplot(ts.plot[ts.plot$year == max(ts.plot$year), ],
                    x = my.x, y = "SpawnBio_re",
                    vert = my.vert, horiz = my.horiz, vert2 = my.vert2,
                    horiz2 = my.horiz2,
                    rel = axis.rel, axes.free = axis.val) +
xlab("Length comps for fishery and survey vs. just fishery") +
ylab("relative error: terminal SSB")
ggsave("termssb.png", dpi = 300)
dev.off()

###############################################################################
###############################################################################
#### Step
#### Correlation plot with RE in terminal SSB
###############################################################################
###############################################################################
growth <- c("L_at_Amin_Fem_GP_1_re", "L_at_Amax_Fem_GP_1_re",
            "VonBert_K_Fem_GP_1_re", "CV_young_Fem_GP_1_re",
            "CV_old_Fem_GP_1_re",
            "SizeSel_1P_1_Fishery_re", "SizeSel_1P_3_Fishery_re",
            "SizeSel_2P_1_Survey_re", "SizeSel_2P_3_Survey_re")
xnames <- c("Length at min age", "Length at max age",
            expression(italic(K)), "CV_young", "CV_old",
            "Fishery selectivity 1","Fishery selectivity 3",
            "Survey selectivity 1", "Survey selectivity 3")

pdf("parcorrelations.pdf")
for (ind in seq_along(growth)) {
  par(mfcol = c(3, 3), las = 1, mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 2),
    tck = -0.015, mgp = c(1.75, 0.45, 0), cex.axis = 1, col.axis = "black")
  for (spp in c("flatfish", "hake", "yellow")) {
    for (my.e in c("int", "ext", "intCV")) {
      my.par <- growth[ind]
      x <- subset(data.plot, A == "bothage" & L == "bothlen" & F == "F1" &
        C != "C20" & D != "D10" & species == spp & E %in% my.e,
        select = my.par)
      y <- subset(ts.plot, year == 100 & A == "bothage" & L == "bothlen" &
        F == "F1"  & C != "C20" & D != "D10" & species == spp & E %in% my.e,
        select = "SpawnBio_re")
      plot(x[, 1], y[, 1], xlim = c(-2, 2), ylim = c(-2, 2), axes = FALSE)
      box(col = "black")
      if (my.e == "int") {
        if (spp == "flatfish") {
          mtext(side = 3, xnames[ind], outer = TRUE, line = 2)
        }
        mtext(side = 3, spp)
      }
      if (spp == "flatfish") {
        axis(2)
        mtext(side = 2, my.e, line = 2, las = 3)
      }
      mod <- lm(y[, 1] ~ x[, 1])
      if (summary(mod)$coefficients[2, 4] < 0.05) {
        abline(mod)
        legend("topright", legend = round(coef(mod)[2], 3), bty = "n")
      }
      abline(v = 0, h = 0, lty = 2, col = gray(0.7))
      legend("topleft", legend = paste("n =", length(x[, 1])), bty = "n")
    }
  axis(1)
  }
}
dev.off()

###############################################################################
###############################################################################
#### Step
#### Time series
###############################################################################
###############################################################################
ggplot(ts[ts$E == "int", ]) +
geom_boxplot(aes(year, SpawnBio_re, group = year)) +
geom_hline(yintercept = 0, col = "red") +
ylab("Relative error in spawning stock biomass") +
xlab("year") +
annotate("text", x = 75, y = 0.4,
  label = "w/ bias adj & int est.")
ggsave("ts_SSB_re.png", dpi = 300)

ggplot(ts[ts$E == "int", ]) +
geom_line(aes(year, SpawnBio_em, group = replicate), size = 0.2, outlier.size = 1) +
geom_line(aes(year, SpawnBio_om, group = replicate), col = "red", lwd = 0.5) +
xlab("year") +
xlab("Spawning stock biomass") +
annotate("text", x = 75, y = max(ts$SpawnBio_om),
  label = "w/ bias adj & int est.")
ggsave("ts_SSB.png", dpi = 300)
dev.off()

###############################################################################
###############################################################################
#### Step
#### Plot data availability
###############################################################################
###############################################################################
source(file.path("..", "lib", "figure_casefiles.R"))

###############################################################################
###############################################################################
#### Step
####
###############################################################################
###############################################################################
source(file.path("..", "lib", "figure_m.R"))
###############################################################################
###############################################################################
#### Step
#### Upon exit return to the original working directory
###############################################################################
###############################################################################
setwd(wd.curr)
