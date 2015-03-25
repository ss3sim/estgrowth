###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-03-25
####Purpose    : Plot results over a range of M values
####Packages   :
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################
library(reshape)
library(reshape2)

add.polygon2 <- function(x, z, level, min = 0, max = 1){
  ## Pass this a series of years (x) and a matrix of trajectories (df) and it
  ## adds a polygon to the current plot with whose area contains 1-level
  # Needs reshape2
  level <- sort(level)
  for (i in 1:length(level)) {
    alpha <- level[i]
    alpha.col <- min + alpha * (max - min)
    col.poly <- rgb(1 - alpha.col, 1 - alpha.col, 1 - alpha.col, alpha = 1)
    quants <- as.matrix(t(apply(z, 2, quantile,
      probs = c(alpha / 2, 1 - alpha / 2), name = FALSE, na.rm = TRUE)))
    polygon(x = c(x, rev(x)), y = c(quants[,1], rev(quants[,2])),
      col = col.poly, border = NA)
  }
}

f2 <- function(data, y, ylim, etraj = paste0("E", c(10:19)), fTraj = c("F1"),
  spp = "hake", casefold = "cases", alphalevels = c(0.05, 0.95)) {
  ## Merge in the values for the E cases
  mvals <- sapply(etraj, function(x) {
    ss3sim:::get_args(file.path(casefold,
      paste0(x, "-", spp, ".txt")))$natM_val[1]
    })
  mvals <- append(mvals, round(mvals[6] * 0.909091, 3), after = 5)
  names(mvals)[6] <- ifelse("E10" %in% names(mvals), "E2", "E1")
  E.df <- data.frame(E = names(mvals), Mfixed = mvals)
  df <- subset(data, E %in% names(mvals) & F %in% fTraj & C == "C0" &
    species %in% spp & D %in% ifelse("E10" %in% names(mvals), "D10", "D0"))
  df <- merge(df, E.df)
  ## Convert to wide format
  df$MfixedFactor <- as.factor(df$Mfixed)
  df.wide <- as.data.frame(cast(data = df, formula = replicate ~ MfixedFactor,
    value = y)[, -1])
  xvals <- as.numeric(names(df.wide))
  plot(0.2, 0.2, type = "n", xlim = range(mvals), ylim = ylim, ann = FALSE,
    axes = FALSE,  yaxs = "i", xaxs = "i")
  add.polygon2(x = xvals, z = df.wide, level = alphalevels, min = 0.15)
  medians <- as.vector(apply(df.wide, 2, median, na.rm = TRUE))
  lines(x = xvals, y = medians, lty = 1, lwd = 2)
  abline(v = mvals[6], h = 0, lty = 2, col = gray(0.7))
}

depletion <- tapply(ts$SpawnBio_em, list(ts$scenario, ts$replicate), function(x) {
  x / mean(x[1:1])
})


metric <- c("depletion_re")
spp <- c("flatfish", "hake", "yellow")
par(mfcol = c(2, length(spp)), mar = c(0, 0, 0, 0), oma = c(2, 3, 4, 3),
    tck = -0.015, mgp = c(0, 0.45, 0), cex.axis = 1, col.axis = "black")
k <- 1


png("figure_m.png")
for(r in seq_along(metric)) {
  for(a in seq_along(spp)) {
    f2(subset(scalars, L == "L30" & A == "A30"), metric[r], ylim = c(-1,8), spp = spp[a])
    mtext(side = 3, line = 0.25, spp[a])
    box(col= "black")
    if (a == 1) {
      axis(2, las = 1)
    }
    if (a == 3) {
      mtext(side = 4, "external", line = 0.25)
    }
    mtext(side = 2, line = 1.5, paste("relative error:", gsub("_re", "", metric)),
      outer = TRUE)
    f2(subset(scalars, L == "L30" & A == "A30"), metric[r],
       ylim = c(-1,1), spp = spp[a], etraj = paste0("E", 20:29))
    axis(1)
    if (a == 1) {
      axis(2, las = 1)
    }
    if (a == 3) {
      mtext(side = 4, "internal", line = 0.25)
    }
    box(col= "black")
  }
}
dev.off()
