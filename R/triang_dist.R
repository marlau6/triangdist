#' @title Density of the Triangular Distribution
#' @description Calculates the probability density function (PDF)
#' for a triangular distribution.
#' @param x Vector of quantiles.
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#' @param mode Mode (c).
#' @return A numeric vector of densities.
#' @export

dtriang <- function(x, min, max, mode) {

  if (any(min > max)) {
    stop("Invalid parameters: min > max")
  } else if (any(mode < min)) {
    stop("Invalid parameters: mode < min")
  } else if (any(mode > max)) {
    stop("Invalid parameters: mode > max")
  }

  n <- max(length(x), length(min), length(max), length(mode))
  x <- rep_len(x, n)
  min <- rep_len(min, n)
  max <- rep_len(max, n)
  mode <- rep_len(mode, n)

  h <- 2 / (max - min)
  y <- rep(0, n)

  a_to_c <- min <= x & mode > x
  a1 <- (2 * (x[a_to_c] - min[a_to_c]))
  a2 <- ((max[a_to_c] - min[a_to_c]) * (mode[a_to_c] - min[a_to_c]))
  y[a_to_c] <- a1 / a2

  c_eq_x <- x == mode
  y[c_eq_x] <- h[c_eq_x]

  c_to_b <- mode < x & max >= x
  c1 <- (2 * (max[c_to_b] - x[c_to_b]))
  c2 <- ((max[c_to_b] - min[c_to_b]) * (max[c_to_b] - mode[c_to_b]))
  y[c_to_b] <- c1 / c2

  y
}


#' @title Distribution Function of the Triangular Distribution
#' @description Calculates the cumulative distribution function (CDF)
#' for a triangular distribution.
#' @param q Vector of quantiles.
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#' @param mode Mode (c).
#' @return A numeric vector of probabilities.
#' @export

ptriang <- function(q, min, max, mode) {

  if (any(min > max)) {
    stop("Invalid parameters: min > max")
  } else if (any(mode < min)) {
    stop("Invalid parameters: mode < min")
  } else if (any(mode > max)) {
    stop("Invalid parameters: mode > max")
  }

  n <- max(length(q), length(min), length(max), length(mode))
  q <- rep_len(q, n)
  min <- rep_len(min, n)
  max <- rep_len(max, n)
  mode <- rep_len(mode, n)
  y <- rep(0, n)

  a_to_c <- q >= min & q < mode
  aeq1 <- ((q[a_to_c] - min[a_to_c]) ^ 2)
  aeq2 <- ((max[a_to_c] - min[a_to_c]) * (mode[a_to_c] - min[a_to_c]))
  y[a_to_c] <- aeq1 / aeq2

  x_eq_c <- mode == q
  y[x_eq_c] <- (mode[x_eq_c] - min[x_eq_c]) / (max[x_eq_c] - min[x_eq_c])

  c_to_b <- q > mode & q <= max
  ceq1 <- (max[c_to_b] - q[c_to_b]) ^2
  ceq2 <- (max[c_to_b] - min[c_to_b]) * (max[c_to_b] - mode[c_to_b])
  y[c_to_b] <- 1 - (ceq1 / ceq2)

  x_bgr_b <- q > max
  y[x_bgr_b] <- 1

  y
}
