#' @title count vowels
#' @description computes the number of vowels of a character string
#' @param x character string
#' @return a named vector with the number of vowels
count_vowels <- function(x) {
  if (!is.string(x)) {
    stop("invalid input; a string was expected")
  } else{
    x = tolower(x)
    return_vec <- c("a" = 0, "e" = 0, "i" = 0, "o" = 0, "u" = 0)
    for (i in 1:nchar(x)) {
      letter = substr(x, i, i)
      if (letter %in% names(return_vec)) {
        return_vec[letter] = return_vec[letter] + 1
      }
    }
    return(return_vec)
  }
}