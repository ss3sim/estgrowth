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

source("lib/plot_functions.R")
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

d <- file.path("casefiles")
## Import the F trajectories for cod
F1 <- eval(parse(text = strsplit(grep("fvals", readLines(file.path(d,
                 "F0-cos.txt")), value = TRUE),";")[[1]][2]))
F2 <- eval(parse(text = strsplit(grep("fvals", readLines(file.path(d,
                 "F1-cos.txt")), value = TRUE),";")[[1]][2]))
F3 <- eval(parse(text = strsplit(grep("fvals", readLines(file.path(d,
                 "F2-cos.txt")), value = TRUE),";")[[1]][2]))
# Get the true Fmsy
Fmsy <- tail(F1, 1)
F1 <- F1/Fmsy; F2 <- F2/Fmsy; F3 <- F3/Fmsy

fudge <- 0.0000 # to fudge the lines vertically a bit
## originally used nice color gradients but reviewer didn't like it so changed
## to dashed lines. CCM 11/7
ltys <- c(1, 2)

make.file(file.type, "figures/Figure1", width=width1, height=3, res=res.png)
  par(mfrow = c(1, 1))
  par(mgp = c(1.25, 0.25, 0), mar = c(2.65, 3.15, 1.5, 0.5),
      col.axis = axis.col, cex.axis = 0.8, tck = -0.01)
  plot(0, 0, xlim = range(axisYears), ylim = c(0, 1.6), type = "n",
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
    text(x = 19 * li, y = 0.6 * li, label = lab[li], col = "black")
  }
  axis(2, at = c(0,1, 1.5), las = 1,
       labels = c(0, expression(italic(F)[MSY]),
                     expression(1.5*italic(F)[MSY])))
  mtext(Fishing~Mortality~(italic(F))~Trends, side = 3, line = 0)
  #add.label("(a)")
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
## Import the data cases
cal <- age <- sapply(c(10, 20), function(t) {
  tname <- paste0("calcomp", t, "-cos.txt")
  treturn <- setNames(lapply(readLines(file.path(d, tname)),
    function(x) {
      temp <- strsplit(x, ";")[[1]][2]
      eval(parse(text = temp))
    }), c("fleets", "years", "Nsamp"))
  treturn
})

age <- sapply(c(10, 30, 31), function(t) {
  tname <- paste0("agecomp", t, "-cos.txt")
  treturn <- setNames(lapply(readLines(file.path(d, tname)),
    function(x) {
      temp <- strsplit(x, ";")[[1]][2]
      eval(parse(text = temp))
    }), c("fleets", "Nsamp", "years", "cpar"))
  treturn
})

len <- sapply(c(10, 30, 31), function(t) {
  tname <- paste0("lcomp", t, "-cos.txt")
  treturn <- setNames(lapply(readLines(file.path(d, tname)),
    function(x) {
      temp <- strsplit(x, ";")[[1]][2]
      eval(parse(text = temp))
    }), c("fleets", "Nsamp", "years", "cpar"))
  treturn
})

index <- setNames(sapply(readLines(file.path(d, "index0-cos.txt")),
  function(x) {
    temp <- strsplit(x, ";")[[1]][2]
    eval(parse(text = temp))
  }), c("fleets", "years", "sd"))

make.file(file.type, "figures/Figure2", width=width2, height=5, res=res.png)
  par(mfrow = c(1, 1), oma = c(1, 1, 1, 0.5), xpd = TRUE,
      mgp = c(1.25, 0.25, 0), mar = c(2.65, 3.15, 2.5, 0.5),
      col.axis = axis.col, cex.axis = 0.8, tck = -0.01)
  plot(0, 0, xlim = c(15, 300), ylim = c(0,0.55), type = "n",
       xlab = "", yaxt = "n", ylab = NA, las = 1, xaxt = "n")
  axis(1, at = c(25, 50, 75, 100, 125, 150, 175, 200, 250, 275, 300),
       label = c(25, 50, 75, 100, 25, 50, 75, 100, 50, 75, 100))
  y <- 0.43

    points(age[3, 2]$years[[2]] + 100,
      rep(y - 0.01, length(age[3, 2]$years[[2]])), pch = 1, col = "black")

  for (ind in 2:3) {
    points(unlist(len[3, ind]$years),
         c(rep(y - ifelse(ind == 2, 0, .22), sapply(len[3, ind]$years, length)[1]),
           rep(y - ifelse(ind == 2, 0.03, 0.25), sapply(len[3, ind]$years, length)[2])),
    pch = unlist(mapply(rep, c(17, 19), sapply(len[3, ind]$years, length))),
    col = "black")

    points(age[3, ind]$years[[2]][c(ifelse(ind == 2, 1, 4), 7)] + 200,
      rep(y + ifelse(ind == 2, 0.07, -0.01), 2),
      pch = ifelse(ind == 2, 24, 21), col = "black", bg = "gray")

   points(unlist(len[3, 1]$years) + ifelse(ind == 2, 0, 100),
         rep(y + ifelse(ind == 2, -0.4, 0.05), sapply(len[3, 3]$years, length)[1]),
         pch = ifelse(ind == 2, 17, 2), col = "black")
  }


  axis(2, at = c(0.05, 0.2, .45), las = 3, tck = 0,
       labels = c("Data poor", "", "Data rich"))
  text(50, y + 0.17, "Length")
  text(165, y + 0.17, "Age")
  text(280, y + 0.17, "Conditional\nage at length")
  mtext(side = 1, "Year", outer = TRUE)
  legend("bottomright", pch = c(17, 2, 24, 19, 1, 21), bty = "n", ncol = 2,
         legend = c("Fishery Length",  "Fishery Age",
                    "Fishery cond.", "Survey Length",
                    "Survey Age", "Survey cond."),
         pt.bg = rep(c("black", "black", "gray"), 2))
  text(c(rep(25, 3), rep(125, 3), rep(250, 5)),
       c(y + c(0.07, -0.18, -0.36, 0.07, 0, -0.06, 0.07, 0.04, -0.01, -0.04, -0.09)),
       c("(A)", "(B)", "(C)", "(A.a)", "(A.b)", "(A.c)",
         "(A.a.1)", "(A.a.2)", "(A.b.1)", "(A.b.2)", "(A.c.1)"))
  box(col = box.col)
my.dev.off()

###############################################################################
###############################################################################
#### Step
#### End of file.
###############################################################################
###############################################################################
setwd(wd.start)
