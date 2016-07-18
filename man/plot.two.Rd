\name{plot.two}
\alias{plot.two}
\docType{package}
\title{plot.two}
\description{
Allows two variables on different scales to be plotted on one set of axes. The scales are added on the left and right sides of the graph with specified labels.
}
\usage{
plot.two(x1, y1, x2, y2, xlim = NULL, ylim1 = NULL, ylim2 = NULL, 
    type1 = "points", type2 = "points", col1 = "red", col2 = "blue", 
    cex = 1.5, pch1 = 16, pch2 = 16, xlab = "", ylab1 = "", ylab2 = "", 
    add = FALSE, xpos = 1)
}
\arguments{
\item{x1,y1,x2,y2}{Data to be plotted}
\item{xlim,ylim1,ylim2}{Optional limits for the 3 axes. If not specified the default limits are used (see \code{\link{pretty}})}
\item{type1,type2}{This should either be points (default) or connected. The latter joins up the points.}
\item{col1,col2}{Colours of each data type to be plotted}
\item{xlab,ylab1,ylab2}{Axis labels}
\item{add}{Allows plotting to preconstructed set of axes. This may be of use if using \code{\link{split.screen}} to generate multiple graphs on a single screen or figure}
}