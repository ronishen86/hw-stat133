#' @title reverse characters
#' @description reverses a string by characters
#' @param x a string
#' @return reversed string
reverse_chars <- function(x) {
  n <- nchar(x)
  return_val <- ""
  for (i in n:1) {
    return_val = paste0(return_val, substr(x, i, i))
  }
  return(return_val)
}