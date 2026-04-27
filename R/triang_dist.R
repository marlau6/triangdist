#' @title Density of the Triangular Distribution
#' @description Calculates the probability density function (PDF) for a triangular distribution.
#' @param x Vector of quantiles.
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#' @param mode Mode (c).
#' @return A numeric vector of densities.
#' @export

dtriang <- function(x, min, max, mode){

  h <- 2 / (max - min)
  m <- (h - 0) / (mode - min)

  for (i in x){
    if (min <= x[i] & max > mode){
      y <- m * (x[i] - min) + 0
    }
  }

}
