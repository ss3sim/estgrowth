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

ssbterm_re <- with(ts, ((SpawnBio_om - SpawnBio_em) / SpawnBio_om))
ts <- cbind(ts, ssbterm_re)

#subset for max_grad
maxgrad <- aggregate(max_grad ~ L + A + E + C + species, data = results_re,
  function(x) c(median(x), sum(x > 0.01)))
data.plot <- subset(results_re, max_grad < 0.01 &
  C %in% c("C0", "C20") & D %in% c("D0", "D10", "D20") &
  E %in% c("fix", "ext", "int"))
ts <- subset(ts[ts$ID %in% data.plot$ID, ],
  C %in% c("C0", "C10","C20") & D %in% c("D0", "D10", "D20") &
  E %in% c("fix", "ext", "int"))

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

plot_scalar_boxplot(ts[ts$year == max(ts$year), ], x = my.x, y = "ssbterm_re",
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
            "CV_old_Fem_GP_1_re", "SR_LN_R0_re",
            "SizeSel_1P_1_Fishery_re", "SizeSel_1P_3_Fishery_re",
            "SizeSel_2P_1_Survey_re", "SizeSel_2P_3_Survey_re")
xnames <- c("Length at min age", "Length at max age",
            expression(italic(K)), "CV_young",
             "CV_old", expression(italic(R0)),
            "Fishery selectivity 1","Fishery selectivity 3",
            "Survey selectivity 1", "Survey selectivity 3")
png("parcorrelations.png", height = 7, width = 7, units = "in", res = 200)
par(mfrow = (c(3, 4)), mar = c(4, 0, 1, 0), oma = c(2, 2, 2, 2))
for(ind in seq_along(growth)) {
  x <- data.plot[data.plot$A != "noage" & data.plot$L == "L30", growth[ind]]
  y <- ts[ts$year == max(ts$year) & ts$A != "noage" & ts$L == "L30", "ssbterm_re"]
  mycol <- apply(data.plot[data.plot$A != "noage" & data.plot$L == "L30", ], 1, function(x) {
    ifelse(x["C"] == "C0", x["D"], x["C"])
  })
  mycol <- as.factor(mycol)
  levels(mycol) <- c("blue", "red", "green", "darkgreen")
  mod <- lm(y ~ mycol / x + 0)
  plot(x, y, col = mycol, ylim = c(-1, 1),
    yaxt = ifelse(ind %in% c(1, 5, 9), "s", "n"),
    xlab = xnames[ind])
  cof <- coef(mod)
    save <- rep(NA, 4)
    for(i in 1:4){
      abline(a = cof[i], b = cof[4 + i], col = mod$xlevels$mycol[i])
      if (summary(mod)$coefficients[4 + i, 4] < 0.05) {
        save[i] <- round(cof[4 + i], 3)
      }
    }
    mtext(side = 3, text = paste(save, collapse = ", "),
      adj = 0, line = -1.5, cex = 0.5)
}
mtext(side = 3, paste("Relative error in terminal SSB vs. ..."), outer = TRUE)
plot(0,0, type = "n", axes = FALSE, bty = "n", xlab = "")
  legend("topright", legend = c("survey cal", "lengths & ages", "fishery mla", "survey mla"),
    col = c("blue", "red", "green", "darkgreen"), pch = 1, bty = "n")
dev.off()

###############################################################################
###############################################################################
#### Step
#### Time series
###############################################################################
###############################################################################
ts$SpawnBio_re <- with(ts, (SpawnBio_om - SpawnBio_em) / SpawnBio_om)
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
source(file.path("..", "lib", "plot_data.R"))

###############################################################################
###############################################################################
#### Step
#### Upon exit return to the original working directory
###############################################################################
###############################################################################
setwd(wd.curr)