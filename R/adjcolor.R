

#' adjColor
#' 
#' Adds two colours together assuming a background colour bg. Acts in a similar way to \code{\link{adjustcolor}} with the exception that alpha is not used. Alpha components are lost when sending coloured text or tables to word through markdown or ReporterRs.
#' 
#' @param col adding colour
#' @param alpha the opacity of alpha, in the range [0,1]
#' @param bg the background colour used as a reference. Default is white,
#' 
adjColor = function(col, alpha = 1, bg = "#FFFFFF") {
  
  .adjColor = function(col, alpha, bg) {
  red = strtoi(substr(col,2,3), base = 16L)
  green = strtoi(substr(col,4,5), base = 16L)
  blue = strtoi(substr(col,6,7), base = 16L)
  
  bg_red = strtoi(substr(bg,2,3), base = 16L)
  bg_green = strtoi(substr(bg,4,5), base = 16L)
  bg_blue = strtoi(substr(bg,6,7), base = 16L)
  
  red = (1-alpha) * bg_red + alpha * red
  blue = (1-alpha) * bg_blue + alpha * blue
  green = (1-alpha) * bg_red + alpha * green
  
  toHex = function(c) {
    cl = as.hexmode(round(c))
    if(nchar(cl) > 1) {
      return(cl)
    }
    return(paste0("0",cl))
  }
  
  paste0("#",toHex(red),toHex(green),toHex(blue))
  }
  
  sapply(col, function(c) .adjColor(c,alpha,bg))
}