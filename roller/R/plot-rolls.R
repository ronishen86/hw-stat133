#' @title Plot of object roll
#' @description Plots the relative frequencies of a series of roll
#' @param x an object of class \code{"roll"}
#' @export
plot.rolls <- function(x, ...) {
  freq <- as.vector(unname(prop.table(table(x$rolls))))
  plot(x$sides, freq, 
       main = paste0("Relative Frequencies in a series of", x$times, "rolls"), 
       xlab = "sides of device",
       ylab = "relative frequencies")
}
