#' @title device
#' @description Creates an object of class \code{"device"}
#' @param sides vector of k ≥ 2 elements, by default numbers 1 and 2.
#' @param prob vector of probabilities for each side (all equal to 1/2 by default), must be the same length as sides.
#' @return an object of class \code{"device"}
#' @export
device <- function(sides = c(1,2), prob = c(1/2, 1/2)) {
  check_sides(sides)
  checks_prob(prob)

  if (length(sides) != length(prob)){
    stop("\n'sides' and 'prob' have different lengths")
  }

  object <- list(
    sides = sides,
    prob = prob
  )
  class(object) = 'device'
  object
}

# private function to check vector of sides
check_sides <- function(sides) {
  if (length(sides) < 2) {
    stop("\n'sides' must be a vector of length greater than 1")
  }
  if (any(duplicated(sides))) {
    stop("\n'sides' cannot have duplicated elements")
  }
  TRUE
}

# private function to check vector of probabilities
check_prob <- function(prob) {
  if (!is.numeric(prob)) {
    stop("\n'prob' must be a numeric vector")
  }
  if (length(prob) < 2) {
    stop("\n'prob' must be a vector of more than one element")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("\n'prob' values must be between 0 and 1")
  }
  if (sum(prob) != 1) {
    stop("\n elements in 'prob' must add up to 1")
  }
  TRUE
}

#' @export
print.device <- function(x, ...) {
  cat('object "device"\n\n')
  cat(" side prob\n")
  for (i in 1:length(x$sides)) {
    cat(i, x$sides[i], x$prob[i], "\n")
  }
  invisible(x)
}

#' @rdname device
#' @param x an R object
#' @export
is.device <- function(x) {
  is(x, 'device')
}
