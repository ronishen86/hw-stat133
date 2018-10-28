#' @title Minkowski
#' @description compute the Minkowski distance of order p (p-norm distance)
#' @param x numeric vector for one point
#' @param y numeric vector for the other point
#' @param p either a numeric value greater than 1, or a character string max (default 1)
#' @return the Minkowski distance
minkowski <- function(x, y, p = 1) {
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  } else if (is.numeric(p)) {
    if (p < 1) {
      stop("p cannot be less than 1")
    } else {
      n <- length(x)
      sum <-  0
      for (i in 1:n) {
        sum = sum + abs(x[i] - y[i]) ^ p
      }
      return(sum ^ (1 / p))
    }
  } else if (is.character("max")) {
      if (p != "max") {
        stop("invalid character value for p")
      } else {
        n <- length(x)
        max <-  0
        for (i in 1:n) {
          if (abs(x[i] - y[i]) > max) {
            max = abs(x[i] - y[i])
          }
        }
        return(max)
      }
    }
}