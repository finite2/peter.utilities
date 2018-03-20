
#' boxDensityPlot
#'
#' This is an alternative plot to hist or plot density. Instead of binning or smoothing each item is drawn around its position +- diff. These are then stacked vertically. A rounding option is built in to give a cleaner building block look.
#'
#' @param dta The data
#' @param diff The width of each item to be plotted in
#' @param rnd a number to round all values to
#' @param add TRUE/FALSE add to an existing plot or not
#' @param xaxs,yaxs,xlim,ylim,ylab Standard plot options
#' @param col the fill colour
#' @param border the border colour
#' @param ... other values passed to the polygon function

#' @export boxDensityPlot
boxDensityPlot = function(dta, type = "p",diff = 0.1, rnd = 0.01, xaxs = "i", yaxs = "i", ylab = "Density", xlab = "test", col = 2, border = 1, add = FALSE, xlim = NULL, ylim = NULL, ...){

  dta = round(dta / rnd) * rnd
  dta_min = round((dta - diff) / rnd) * rnd
  dta_max = round((dta + diff) / rnd) * rnd

  multiplier = 1
  if(type == "p") {
    multiplier = 1/length(dta)
  }

  x = rep(0,0)
  y = rep(0,0)
  for(i in sort(unique(c(dta,dta_min,dta_max)))){
    x = c(x, i,i)
    y = c(y, sum(i > dta_min  & i <= dta_max, na.rm = TRUE), sum(i >= dta_min  & i < dta_max, na.rm = TRUE))
  }

  y = y/(2*diff)*multiplier
  if(!add){

    if(is.null(xlim)){
      xlim = range(pretty(x))
    }
    if(is.null(ylim)){
      ylim = range(pretty(y))
    }

    blankPlot(xlim = xlim, ylim = ylim, xaxs = xaxs, yaxs = yaxs)
    axis(1)
    axis(1, at = xlim,labels = FALSE, tck = 0)
    mtext(xlab, side = 1, line = 2.5)
    axis(2, las = 2)
    mtext(ylab, side = 2, line = 2.5)
  }
  polygon(x,y, col = col, border = border, ...)

}
