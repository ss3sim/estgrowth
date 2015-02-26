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
resultfiles <- list.files(dir.dropbox, pattern = ".csv",
  full.names = TRUE, recursive = TRUE)
scalars <- do.call("rbind",
  lapply(grep("scalar", resultfiles, value = TRUE), read.csv))
ts <- do.call("rbind",
  lapply(grep("ts", resultfiles, value = TRUE), read.csv))
# Calculate the relative error for scalars
results_re <- calculate_re(scalars, FALSE)

###############################################################################
###############################################################################
#### Step
#### Name cases
###############################################################################
###############################################################################

results_re <- change_levels(data = results_re, group = "E",
  old = c("E0", "E1", "E2"), new = c("fix", "int", "ext"))
ts <- change_levels(data = ts, group = "E",
  old = c("E0", "E1", "E2"), new = c("fix", "int", "ext"))

results_re <- change_levels(data = results_re, group = "A",
  old = c("A0", "A10", "A30"), new = c("noage", "fishage", "bothage"))
ts <- change_levels(data = ts, group = "A",
  old = c("A0", "A10", "A30"), new = c("noage", "fishage", "bothage"))

###############################################################################
###############################################################################
#### Step
#### Set up plots and subset data
###############################################################################
###############################################################################
axis.val <- TRUE
axis.rel <- TRUE
my.horiz <- "E"
my.horiz2 <- "C"
my.vert <- "A"
my.vert2 <- "species"
my.x <- "L"

#subset for max_grad
maxgrad <- aggregate(max_grad ~ L + A + E + C, data = results_re,
  function(x) c(median(x), sum(x > 0.01)))
data.plot <- subset(results_re[results_re$max_grad < 0.01, ],
  C %in% c("C0", "C20") & D %in% c("D0", "D10", "D20") &
  E %in% c("ext", "int"))
identifer <- with(data.plot, paste0(scenario, replicate))
ts <- subset(ts[with(ts, paste0(scenario, replicate)) %in% identifer, ],
  C %in% c("C0", "C10","C20") & D %in% c("D0", "D10", "D20") &
  E %in% c"ext", "int"))

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


data.plot <- subset(results_re[results_re$max_grad < 0.01, ],
  C %in% c("C0", "C20") & D %in% c("D0", "D10", "D20") &
  E %in% c("fix", "ext", "int"))

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

###############################################################################
###############################################################################
#### Step
#### Time series
###############################################################################
###############################################################################
ts$SpawnBio_re <- with(ts, (SpawnBio_om - SpawnBio_em) / SpawnBio_om)
ggplot(ts) +
geom_boxplot(aes(year, SpawnBio_re, group = year)) +
geom_hline(yintercept = 0, col = "red") +
ylab("Relative error in spawning stock biomass") +
xlab("year") +
annotate("text", x = 75, y = 0.4,
  label = "100 Deterministic Runs with bias adj.\nData = Length / Age / CAL; Pars = Fixed Growth")
ggsave("ts_SSB_re.png", dpi = 300)

ggplot(ts) +
geom_line(aes(year, SpawnBio_em, group = replicate), size = 0.2, outlier.size = 1) +
geom_line(aes(year, SpawnBio_om, group = replicate), col = "red", lwd = 2) +
xlab("year") +
xlab("Spawning stock biomass") +
annotate("text", x = 75, y = max(ts$SpawnBio_om),
  label = "100 Deterministic Runs with bias adj.\nData = Length / Age / CAL; Pars = Fixed Growth")
ggsave("ts_SSB.png", dpi = 300)

###############################################################################
###############################################################################
#### Step
#### Plot data availability
###############################################################################
###############################################################################
source(file.path("..", "lib", "plot_data.R"))

###############################################################################
###############################################################################
#### Step
#### Upon exit return to the original working directory
###############################################################################
###############################################################################
setwd(wd.curr)