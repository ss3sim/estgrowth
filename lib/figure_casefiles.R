###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2014-01-29
####Purpose    : Create figures representing the study design for CAPAM Est
####           : growth paper
####Packages   : cumplyr, plyr, reshape, reshape2, ss3sim
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################

###############################################################################
## Step 01
## Set up the variable inputs
###############################################################################
## pdf requires inches for the widths and heights:
width2 <- 6.69291                       # 170mm
width1 <- 3.34646                       # 85mm

file.type = "png"
res.png <- 500
axis.col <- 1 #gray(.3)
box.col  <- 1 #gray(.3)
alpha.levels <- c(0.1, 0.5)

###############################################################################
## Step 02
## Set working directory and source the plotting functions
###############################################################################
wd.start <- getwd()
pathnames <- tail(unlist(strsplit(wd.start, "/")), 1)
if ("estgrowth" != pathnames) {
  stop("check your path, estgrowth directory was not found")
}

source(file.path("lib", "plot_functions.R"))
add.label <- function(label, ...) {
  legend("topleft", legend = " ", title = label, bty = 'n', ...)
}

###############################################################################
## Step 03
## install the packages
###############################################################################
## devtools::install_github("ss3sim", username="seananderson")
library(ss3sim)
library(ss3models)
library(reshape2)
library(cumplyr)
library(plyr)

###############################################################################
###############################################################################
## FIGURES FOR THE MAIN TEXT
###############################################################################
###############################################################################

###############################################################################
###############################################################################
#### Step
#### FIGURE 1: F patterns over time
###############################################################################
###############################################################################
axisYears <- 1:100
## cols <- paste0("grey", c(75, 40, 10)); cols <- c("black", rep(cols, 2))
cols <- rep("black", 7)

d <- file.path("cases")
my.spp <- c("hake", "flatfish", "yellow")

# Get the true Fmsy
fmsy <- download.file("https://raw.githubusercontent.com/ss3sim/ss3models/master/extra/fmsytable.csv",
  "fmsy.csv", method = "curl")
fmsy <- read.csv("fmsy.csv", header = TRUE)
done <- file.remove("fmsy.csv")

make.file(file.type, "figures/Figure1", width = width1, height = 3,
  res = res.png)
par(mfcol = c(3, 2), oma = c(4, 4, 4, 4), mar = c(0, 0, 0, 0),
  mgp = c(1.25, 0.25, 0), col.axis = axis.col, cex.axis = 0.8, tck = -0.01)

# Fishing patterns
for (spp in seq_along(my.spp)) {
  plot(0, ylim = c(0, 1.75), xlim = range(axisYears), las = 1, xaxt = "n",
    xlab = "", type = "n", ylab = "", yaxt = "n")
  add.label(my.spp[spp])
  axis(2, at = c(0,1, 1.5), las = 1,
    labels = c(0, expression(italic(F)[MSY]), expression(1.5*italic(F)[MSY])))
  if (spp == 3) axis(1)
  lines(axisYears,
    ss3sim:::get_args(file.path(d, paste0("F0-", my.spp[spp], ".txt")))$fvals /
    fmsy[fmsy$species == my.spp[spp], "fmsy"],
    lty = 1, lwd = 1.5)
  lines(axisYears,
    ss3sim:::get_args(file.path(d, paste0("F1-", my.spp[spp], ".txt")))$fvals /
    fmsy[fmsy$species == my.spp[spp], "fmsy"],
    lty = 2, lwd = 1.5)
  if (spp == 1) {
    legend("top", lty = c(1, 2), c("constant", "contrast"), bty = "n")
    mtext(Fishing~Mortality~(italic(F)), side = 3, line = 0)
  }
}

# Biology
# for (spp in seq_along(my.spp)) {
#   scen <- paste0("D100-F1-", my.spp[spp])
#   unlink(scen, recursive = TRUE)
#   done <- file.copy(system.file("cases", "index100-cod.txt", package = "ss3models"),
#             gsub("cod", my.spp[spp], system.file("cases", "index100-cod.txt", package = "ss3models")))
#   done <- file.copy(system.file("cases", "lcomp100-cod.txt", package = "ss3models"),
#             gsub("cod", my.spp[spp], system.file("cases", "lcomp100-cod.txt", package = "ss3models")))
#   done <- file.copy(system.file("cases", "agecomp100-cod.txt", package = "ss3models"),
#             gsub("cod", my.spp[spp], system.file("cases", "agecomp100-cod.txt", package = "ss3models")))

#   run_ss3sim(1, scen, system.file("cases", package = "ss3models"),
#     ss3model(my.spp[spp], "om"), ss3model(my.spp[spp], "em"),
#     show.output.on.console = FALSE)
#   setwd(file.path(scen, "1", "om"))
#   res <- list()
#   res[[10]] <- r4ss::SS_output(getwd(), covar = FALSE, printstats = FALSE,
#     verbose = FALSE)
#   L <- unique(sapply(strsplit(grep("E[1][1-9]",
#     dir(file.path("..", "..", "..", "cases")), value = TRUE), "-"), "[", 1))

#   plot(0, xaxt = "n", yaxt = "n", xlab = "", ylab = "",
#     xlim = range(axisYears), ylim = c(0, 1.1), type = "n")
#   lines(axisYears, res[[10]]$sprseries$spr[axisYears], lwd = 2)
#   if (spp == 1) {
#     axis(2, las = 1)
#     mtext(Spawner~Per~Recruit~(italic(SPR)), side = 2, line = 2.5)
#   }
#   for (l in seq_along(L)) {
#     par <- readLines("ss3.par")
#     par[grep(" MGparm[1]:", par, fixed = TRUE) + 1] <- ss3sim:::get_args(
#       file.path("..", "..", "..", "cases", paste0(L[l], "-",
#       my.spp[spp], ".txt")))$natM_val[1]
#     writeLines(par, "ss3.par")
#     system("ss3 -nohess", show.output.on.console = FALSE)

#     res[[l]] <- r4ss::SS_output(getwd(), covar = FALSE, printstats = FALSE,
#       verbose = FALSE)
#     lines(axisYears, res[[l]]$sprseries$spr[axisYears])
#     if (l == 1) abline(h = res[[l]]$sprtarg, lty = 2)
#   }
#   setwd(file.path("..", "..", ".."))
# }

# Data
  vals <- ss3sim:::get_args(file.path(d, paste0("agecomp30-", my.spp[spp], ".txt")))$years
  plot(0, ylim = c(0, 5), xlim = range(axisYears), type = "n", yaxt = "n", xlab = "", ylab = "",
     xaxt = "n")
  add.label("fishery")
  axis(2, at = seq(0.5, 4.5, by = 1), labels = letters[1:5], las = 1)
  mtext("Data", side = 3)
  legend("to", pch = c(1, 19, 2), legend = c("age-composition", "length-composition", "survey index"),
    bty = "n")

  points(x = vals[[2]], y = rep(0.50, length(vals[[2]])), pch = 19)

  points(x = vals[[1]], y = rep(1.25, length(vals[[1]])))
  points(x = vals[[1]], y = rep(1.50, length(vals[[1]])), pch = 19)

  points(x = vals[[1]], y = rep(2.25, length(vals[[1]])))
  points(x = vals[[1]], y = rep(2.50, length(vals[[1]])), pch = 19)

  points(x = vals[[1]], y = rep(3.25, length(vals[[1]])))
  points(x = vals[[1]], y = rep(3.50, length(vals[[1]])), pch = 19)

  points(x = vals[[1]], y = rep(4.25, length(vals[[1]])))
  points(x = vals[[1]], y = rep(4.50, length(vals[[1]])), pch = 19)

  plot(0, ylim = c(0, 5), xlim = range(axisYears), type = "n", yaxt = "n", xlab = "", ylab = "",
     xaxt = "n")
  add.label("survey")
  points(x = vals[[2]], y = rep(0.50, length(vals[[2]])), pch = 19)

  points(x = vals[[2]], y = rep(2.50, length(vals[[2]])), pch = 19)

  points(x = vals[[2]], y = rep(3.25, length(vals[[2]])))

  points(x = vals[[2]], y = rep(4.25, length(vals[[2]])))
  points(x = vals[[2]], y = rep(4.50, length(vals[[2]])), pch = 19)

  plot(0, ylim = c(0, 5), xlim = range(axisYears), type = "n", yaxt = "n", xlab = "", ylab = "")
  add.label("index")
  for(ind in 1:5) {
    points(ss3sim:::get_args(file.path(d, paste0("index0-", my.spp[spp], ".txt")))$years[[1]],
      y = rep(seq(0.5, 4.5, by = 1)[ind], 13), pch = 2)
  }
my.dev.off()
## End Figure 1
## ------------------------------------------------------------


###############################################################################
###############################################################################
#### Step
#### End of file.
###############################################################################
###############################################################################
setwd(wd.start)
