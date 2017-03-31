#' blank Plots
#'
#' This is a convinience wrapper function for creating a blank plot. You only need to suppy limits to initialise the plot. Then add to the plot using axis, points, lines, mtext, legend etc.
#'
#' @param xlim The x axis range
#' @param ylim The y axis range
#' @param xaxs Either "i" or "r". i fits the range exactly whilst r add a little white space to either end. Default is i here which is the reverse of plot().
#' @param yaxs Either "i" or "r". i fits the range exactly whilst r add a little white space to either end. Default is i here which is the reverse of plot().
#'
#' @export blankPlot
blankPlot = function(xlim = c(0,1), ylim = c(0,1), xaxs = "i", yaxs = "i"){
  plot(0,0,axes = FALSE, xlim = xlim, ylim = ylim, xlab = "", ylab = "", xaxs = xaxs, yaxs = yaxs, type = "n")
}
