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
pathnames <- unlist(strsplit(wd.start, "/"))
if (!"estgrowth" %in% pathnames) {
  stop("check your path, estgrowth directory was not found")
}
wd.shouldbe <- paste(pathnames[1:grep("estgrowth", pathnames)], collapse = "/")

source("../lib/plot_functions.R")
add.label <- function(label, ...) {
  legend("topleft", legend = " ", title = label, bty = 'n', ...)
}

###############################################################################
## Step 03
## install the packages
###############################################################################
## devtools::install_github("ss3sim", username="seananderson")
library(ss3sim)
library(reshape2)
library(cumplyr)
library(plyr)

###############################################################################
## Step 04
## Read in the data, may take a few minutes it is large
###############################################################################
# ts      <- read.csv("../../Result Files/ts_ICESJMS-2013-347.csv")
# scalars <- read.csv("../../Result Files/scalar_ICESJMS-2013-347.csv")
# terminal <- subset(ts, year == 99)

###############################################################################
## Step 05
## Create a table of scenarios ran
###############################################################################
# counts.long <- ddply(subset(scalars,
#                             !(F.num %in% c("F11", "F31"))),
#                      .(M.num, E.num, F.num, species), summarize,
#                      length(F.num))
# counts.wide <- cast(counts.long, M.num + E.num + F.num ~ species,
#                     value = "..1")
# counts.wide[is.na(counts.wide)] <- 0
# write.table(counts.wide, "../resultfiles/counts_by_scenario.csv",
#             sep = ",", row.names = FALSE)


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
axisYears <- 1:100  # As referenced in paper rather than 'real' years
emYears <- 1:99     # The EM is only fed 99 years of data
## cols <- paste0("grey", c(75, 40, 10)); cols <- c("black", rep(cols, 2))
cols <- rep("black", 7)

d <- file.path("..", "casefiles")
## Import the F trajectories for cod
F1 <- eval(parse(text = strsplit(grep("fvals", readLines(file.path(d,
                 "F0-cos.txt")), value = TRUE),";")[[1]][2]))
F2 <- eval(parse(text = strsplit(grep("fvals", readLines(file.path(d,
                 "F1-cos.txt")), value = TRUE),";")[[1]][2]))
# Get the true Fmsy
Fmsy <- tail(F1, 1)
F1 <- F1/Fmsy; F2 <- F2/Fmsy; F3 <- F3/Fmsy

fudge <- 0.0000 # to fudge the lines vertically a bit
## originally used nice color gradients but reviewer didn't like it so changed
## to dashed lines. CCM 11/7
ltys <- c(1, 2)
ltsy <- c(1, rep(ltys, 2))

make.file(file.type, "../figures/Figure1", width=width1, height=3, res=res.png)
  par(mfrow = c(1, 1))
  par(mgp = c(1.25, 0.25, 0), mar = c(2.65, 3.15, 1.5, 0.5),
      col.axis = axis.col, cex.axis = 0.8, tck = -0.01)
  plot(0, 0, xlim = range(axisYears), ylim = c(0,1.5), type = "n",
       xlab = "Year", yaxt = 'n', ylab = NA, las = 1)
  for (li in 1:2) {
    if (li == 1) {
      y <- F1[emYears]
    }
    if (li == 2) {
      y <- F2[emYears]
    }
    lab <- c("Constant", "Contrast")
    lines(emYears, y, lty=ltys[li], lwd=2, col = "black")
    text(x = 18 * li, y = 0.7 * li, label = lab[li], col = "black")
  }
  #legend("topleft", legend=Fnames, lty=ltys, bty='n', lwd=2)
  axis(2, at = c(0,1, 1.5), las = 1,
       labels = c(0, expression(italic(F)[MSY]),
                     expression(1.5*italic(F)[MSY])))
  mtext(Fishing~Mortality~(italic(F))~Trends, side = 3, line = 0)
  add.label("(a)")
  box(col = box.col)
my.dev.off()
## End Figure 1
## ------------------------------------------------------------

###############################################################################
###############################################################################
#### Step
#### Figure 2
###############################################################################
###############################################################################
make.file(file.type, "../figures/Figure2", width=width1, height=3, res=res.png)
  par(mfrow = c(1, 1))
  par(mgp = c(1.25, 0.25, 0), mar = c(2.65, 3.15, 1.5, 0.5),
      col.axis = axis.col, cex.axis = 0.8, tck = -0.01)
  plot(0, 0, xlim = range(axisYears), ylim = c(0,1.5), type = "n",
       xlab = "Year", yaxt = 'n', ylab = NA, las = 1)
  y <- 1
  points(all.fish, rep(y, length(all.fish)), pch = 1, col = "black")
    text(x = 10, y = y, label = "Fishery agecomp", col = "black")
  y <- y - 0.1
  points(all.surv, rep(y, length(all.surv)), pch = 1, col = "black")
    text(x = 10, y = y, label = "Survey agecomp", col = "black")

  #legend("topleft", legend=Fnames, lty=ltys, bty='n', lwd=2)
  axis(2, at = c(0,1, 1.5), las = 1,
       labels = c(0, expression(italic(F)[MSY]),
                     expression(1.5*italic(F)[MSY])))
  mtext(Fishing~Mortality~(italic(F))~Trends, side = 3, line = 0)
  add.label("(a)")
  box(col = box.col)
my.dev.off()

###############################################################################
###############################################################################
#### Step
#### End of file.
###############################################################################
###############################################################################
setwd(wd.start)
