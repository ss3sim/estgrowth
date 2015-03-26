library(ggplot2)
library(lubridate)
require(xtable)
library(reshape)
library(reshape2)

make.file <- function(type=c("png","pdf", "none"), filename, width, height, res){
    ## Pass it file type and dimensions in inches. It creates file.
    type <- match.arg(type)
    ## If no extension given, add one
    if(length(grep(type, filename))==0)
        filename <- paste0(filename,".",type)
    if(type=="png") png(filename, width=width, height=height, units="in", res=res)
    else if(type=="pdf"){pdf(filename, width=width, height=height)}
    else if(dev.cur()==1) dev.new(width=width, height=height)
}
my.dev.off <- function(){
    xx <- names(dev.cur())
    if(length(grep("png", xx))==1 | length(grep("pdf",xx))==1)
        dev.off()
}


add.polygon <- function(data=ts,  y, xvals, alpha.level, alpha.min=0, alpha.max=1){
    ## Pass this a series of years (x) and a matrix of trajectories (df) and it
    ## adds a polygon to the current plot with whose area contains 1-alpha.level
    library(reshape2)
    data <- data[order(data$year),]
    df.wide <- cast(data=data, formula=replicate~year, value=y)[,-1]
    x <- as.numeric(names(df.wide))
    alpha.level <- sort(alpha.level)
    for(i in 1:length(alpha.level)){
        alpha <- alpha.level[i]
        alpha.col <- alpha.min+alpha*(alpha.max-alpha.min)
        col.poly <- rgb(1-alpha.col,1-alpha.col,1-alpha.col, alpha=1)
        quantiles.temp <-  as.matrix(t(apply(df.wide, 2, quantile,
                                             probs=c(alpha/2,1-alpha/2),name=F, na.rm=T)))
        polygon(x=c(x, rev(x)), y=c(quantiles.temp[,1], rev(quantiles.temp[,2])),
                col=col.poly, border=NA)
    }
    return(invisible(df.wide))
}

add.polygon2 <- function(x, y, z, alpha.level, alpha.min=0, alpha.max=1){
    ## Pass this a series of years (x) and a matrix of trajectories (df) and it
    ## adds a polygon to the current plot with whose area contains 1-alpha.level
    library(reshape2)
    alpha.level <- sort(alpha.level)
    for(i in 1:length(alpha.level)){
        alpha <- alpha.level[i]
        alpha.col <- alpha.min+alpha*(alpha.max-alpha.min)
        col.poly <- rgb(1-alpha.col,1-alpha.col,1-alpha.col, alpha=1)
        quantiles.temp <-  as.matrix(t(apply(z, 2, quantile,
                                             probs=c(alpha/2,1-alpha/2),name=F, na.rm=T)))
        polygon(x=c(x, rev(x)), y=c(quantiles.temp[,1], rev(quantiles.temp[,2])),
                col=col.poly, border=NA)

    }
}

change_levels <- function(data, group, old, new) {
  true <- levels(data[, group])
  true.which <- sapply(old, function(x) grep(paste0(x, "$"), true))
  if (is.list(true.which)) stop("Not all levels found in data")
  levels(data[, group])[as.integer(true.which)] <- new
  invisible(return(data))
}
