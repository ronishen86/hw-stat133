#' @title [
#' @description extract the value of a given roll
#' @param x an object of class \code{"roll"}
#' @param i an integer
#' @export
'[.roll' <- function(x, i) {
  x$rolls[i]
}

#' @title [<- 
#' @description replace the value of a given roll.
#' @param x an object of class \code{"roll"}
#' @param i an integer
#' @param value an element in sides 
#' @export
'[<-.roll' <- function(x, i, value) {
  if (!(is.element(value, x$sides))) {
    stop(sprintf('\nreplacing value must be %s or %s', x$coin$sides[1], x$sides$coin[2]))
  }
  if (i > x$total) {
    stop("\nindex out of bounds")
  }
  x$rolls[i] <- value
  make_roll(device(x$sides, x$prob), x$rolls)
}

#' @title + 
#' @description add more rolls.
#' @param x an object of class \code{"roll"}
#' @param j an interger
#' @export
'+.roll' <- function(x, j) {
  if (length(j) != 1 | j <= 0) {
    stop("\ninvalid increment")
  }
  more_throw <- throw(device(x$sides, x$prob), times = j)
  make_roll(device(x$sides, x$prob), c(x$rolls, more_throw))
}