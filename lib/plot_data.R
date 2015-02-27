library(devtools)
install_github("ss3sim/ss3sim", ref = "master")
library(ss3sim)
wd.curr <- getwd()

full <- function(data, years = 1:100) {
  years <- data.frame("year" = years)
  if (length(data[[1]]) > 1) {
    data <- with(data, 
      mapply(cbind, years, Nsamp, mapply(list, fleets)))
    data <- lapply(data, function(i) {
      colnames(i) <- c("year", "n", "fleet")
      temp <- merge(years, i, by = "year", all = TRUE)
      return(temp)
    })
    data <- do.call("rbind", sapply(data, "[", 2))
  } else {
    data <- with(data, data.frame(years, Nsamp, list(fleets)))
    colnames(data) <- c("year", "n", "fleet")
    data <- merge(years, data, by = "year", all = TRUE)
    data <- rbind(data[, 2])
  }
  return(data)
}

setwd(file.path(dir.main, "cases"))
A30 <- full(ss3sim:::get_args("agecomp30-flatfish.txt"))
A10 <- full(ss3sim:::get_args("agecomp10-flatfish.txt"))
L30 <- full(ss3sim:::get_args("lcomp30-flatfish.txt"))
L10 <- full(ss3sim:::get_args("lcomp10-flatfish.txt"))
set.seed(2)
C20 <- full(ss3sim:::get_args("calcomp20-flatfish.txt"))
set.seed(2)
D20 <- full(ss3sim:::get_args("mlacomp20-flatfish.txt"))
set.seed(2)
D10 <- full(ss3sim:::get_args("mlacomp10-flatfish.txt"))
setwd(wd.curr)

p.1 <- rbind(L10, NA, NA,  NA, NA, NA, NA)
p.2 <- rbind(L10, NA, A10, NA, NA, NA, NA)
p.3 <- rbind(L10, NA, A30,     NA, NA, NA)
p.4 <- rbind(L10, NA, A10, NA, NA, D10, NA)

p.5 <- rbind(L30,     NA,  NA, NA, NA, NA)
p.6 <- rbind(L30,     A10, NA, NA, D10, NA)
p.7 <- rbind(L30,     A10, NA, NA, NA, NA)
p.8 <- rbind(L30,     A30,     NA, NA, NA)

p.9 <- rbind(L30,     A30,     C20, NA, NA)

p.10 <- rbind(L30,    A30,     NA,  D10, NA)
p.11 <- rbind(L30,    A30,     NA,  NA, D20)

data <- eval(parse(text = paste("list(",
  paste(paste("p", 1:11, sep = "."), collapse = ","), ")")))

png(file.path("results", "data.png"),
  height = 12, width = 7, units = "in", res = 200)
dens <- 50
myhorz <- TRUE
par(mfcol = c(40, 2), oma = c(2.5, 2.5, 1, 1), mar = rep(0, 4),
	xpd = TRUE)
sapply(c(21:60, 61:100), function(x) {
	data.plot <- lapply(data, function(y) {
		temp <- y[, x]
		return(temp)
	})
	data.plot <- do.call("cbind", data.plot)
	data.plot[is.na(data.plot)] <- 0
	if (all(data.plot == 0)) {
	  barplot(data.plot[1,1], axes = FALSE, col = "white",
	  	xlim = c(0, 500), border = "white", ylim = c(0, 1),
	  	width = 0.000001, horiz = myhorz)
	} else {
	barplot(data.plot, yaxt = "n", xaxt = "n", xlim = c(0, 500),
	  border = "white", horiz = myhorz,
	  col = c(2,2,3,3,4,5,6), 
	  density = c(-1, dens, -1, dens, -1, -1, -1))
	if (x == 80) box(lwd = 2.5)
	}
})

  par(fig = c(0, 0.5, 0, 1), new = TRUE)
  x <- seq(20, 60, by = 5)
  plot(rep(0.005, length(x)), x, type = "n", bty = "n", axes = FALSE)
  text(0.003, 58, cex = 1.5, xpd = TRUE, label = "years", srt = -90)
  axis(2, at = x - 0.75, rev(x), las = 1)
  legend(0.003, at = 41, legend = "years")
 	legend("topright", inset=c(0.2,0.15), bty = "o", 
  	  fill = c(2,2,3,3,4,5,6), 
  	  xpd = TRUE, cex = 1.35, 
  	  border = "white", box.lwd = 0, box.col = "white",
  	  density = c(NA, dens, NA, dens, NA, NA, NA),
      legend = c("length comp: fishery", "length comp: survey",
  	             "age comp: fishery", "age comp: fishery",
  	             "cond age-at-length: survey",
  	             "mean length-at-age: fishery", 
  	             "mean length-at-age: survey"))
  	mtext(side = 3, "sample size", line = -3, outer = TRUE, adj = 0)

  par(fig = c(0.5, 1, 0, 1), new = TRUE)
  x <- seq(60, 100, by = 5)
  plot(rep(0.5, length(x)), x, type = "n", bty = "n", axes = FALSE)
  axis(2, at = x - 0.75, rev(x), las = 1)

 
x <- 80
par(fig = c(0.25,0.45,0.0,0.65), new = TRUE, mar = c(2, 2, 0, 0))
barplot(sapply(data, function(y) {
		temp <- y[, x]
		temp[is.na(temp)] <- 0
		return(temp)}), col = c(2,2,3,3,4,5,6), 
  density=c(-1,dens,-1,dens,-1,-1, -1), las = 1)
box()
mtext(side = 3, paste("year", x), line = -2, cex = 0.95, adj = 0)
mtext(side = 1, paste(letters[1:11], collapse = " "), cex = 0.75)

dev.off()
