#' @title is hex 
#' @description checks whether an input string is a valid hex color without an alpha transparency value
#' @param x a string input
#' @return TRUE or FALSE
is_hex <- function(x) {
  if (!is.string(x)) {
    stop("invalid input; a string was expected")
  } else if (substr(x, 1, 1) != "#" || nchar(x) != 7) {
    return(FALSE)
  } else {
    x = toupper(x)
    valid = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", LETTERS[1:6])
    for (i in 2:7) {
      if (!(substr(x, i, i) %in% valid)) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
}


#' @title is hex alpha
#' @description checks whether an input string is a valid hex color with an alpha transparency value
#' @param x a string input
#' @return TRUE or FALSE
is_hex_alpha <- function(x) {
  if (!is.string(x)) {
    stop("invalid input; a string was expected")
  } else if (substr(x, 1, 1) != "#" || nchar(x) != 9) {
    return(FALSE)
  } else {
    x = toupper(x)
    valid = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", LETTERS[1:6])
    for (i in 2:9) {
      if (!(substr(x, i, i) %in% valid)) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
}