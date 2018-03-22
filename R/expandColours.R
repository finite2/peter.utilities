


#' expand Colours
#'
#' A function for linearly interpolating a vector of colour
#' @param cols a vector of hex colours.
#' @param count the number of colours to return.
#'
#'
#' @export expandColours
expandColours = function(cols, count) {
  interpolateColour = function(cols, dist) {

    i = floor(dist)
    j = ceiling(dist)
    prop = dist %% 1
    if(i == j) {
      return(cols[i])
    }
    red = (1-prop)*strtoi(substr(cols[i],2,3),16L) + prop*strtoi(substr(cols[j],2,3),16L)
    green = (1-prop)*strtoi(substr(cols[i],4,5),16L) + prop*strtoi(substr(cols[j],4,5),16L)
    blue = (1-prop)*strtoi(substr(cols[i],6,7),16L) + prop*strtoi(substr(cols[j],6,7),16L)
    return(rgb(red/255,green/255,blue/255))
  }

  distance = (length(cols)-1)/(count-1)
  return(sapply(1+(0:(count-1))*distance, function(dist) interpolateColour(cols, dist)))
}
