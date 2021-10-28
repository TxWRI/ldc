#' Calculate the geomean
#'
#' Calculates the geometric mean of a vector of positive values
#' @param x vector of positive values
#' @param na.rm logical
#'
#' @return numeric value
#' @noRd
#' @keywords internal
#' @import units
#'
Gmean <- function(x, na.rm=TRUE){

  x <- as.numeric(x)

  y <- exp(mean(log(x), na.rm = na.rm))

  return(y)
}
