#' @title roll a device
#' @description Creates an object of class \code{"roll"}
#' @param device object of class "device"
#' @param times number of times to roll the device (default value of 1)
#' @return an object of class \code{"roll"} with the following elements:
#' @return \item{rolls}{vector of rolls}
#' @return \item{sides}{vector of device \code{"sides"}}
#' @return \item{prob}{vector of device \code{"prob"}}
#' @return \item{total}{total number of rolls}
#' @export
roll <- function(device, times = 1) {
  check_times(times)

  rolls <- throw(device, times)
  make_roll(device, rolls)
}

# private function
throw <- function(x, times = 1) {
  sample(x$sides, size = times, replace = TRUE, prob = x$prob)
}

#' @title Make Roll Object
#' @description Constructor function for object "roll"
#' @param device object of class device
#' @param throw object of class throw
#' @keywords internal
make_roll <- function(device, throw) {
  res <- list(
    rolls = throw,
    sides = device$sides,
    prob = device$prob,
    total = length(throw))

  class(res) <- 'roll'
  res
}

# private function to check vector of 'times'
check_times <- function(times) {
  if (!is.numeric(times) | times < 1) {
    stop("'times' must be a positive integer greater than or equal to 1")
  }
  TRUE
}

#' @export
print.roll <- function(x) {
  cat('object "rolls"\n\n')
  cat('$rolls\n')
  cat(x$rolls)
  invisible(x)
}

#' @export
summary.roll <- function(roll) {
  freqs <- data.frame(
    side = roll$sides,
    count = as.vector(unname(table(roll$rolls))),
    prop = as.vector(unname(prop.table(table(roll$rolls)))))
  obj3 <- list(freqs = freqs)
  class(obj3) <- "summary.roll"
  obj3
}

#' @export
print.summary.roll <- function(sum, ...) {
  cat('summary "roll"\n\n')
  cat("  side count prob\n")
  for (i in 1:nrow(sum[[1]])) {
    cat(i, toString(sum[[1]]$side[i]), " ", sum[[1]]$count[i], " ", sum[[1]]$prop[i], "\n")
  }
  invisible(sum)
}


