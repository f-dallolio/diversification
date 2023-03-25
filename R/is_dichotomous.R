#' Check for dichotomous variables
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_dichotomous <- function(x) {
  n_unique_x <- length(unique(x))
  if (n_unique_x == 2) {
    if (all(x == 0 | x == 1)) {
      return(TRUE)
    } else {
      warning("A predictor might be dichotomous but not 0|1.")
    }
  }
  FALSE
}

is_categorical <- function(x) {
  if(is.factor(x) | is.character(x) | is_dichotomous(x)){
    return(TRUE)
  }
  n_unique_x <- length(unique(x))
  if (n_unique_x > 2 & n_unique_x < 8) {
    warning(
      paste(
        "A predictor has between 3 and 7 unique values.",
        "It might be categorical with multiple categories",
        "but without dummy coding."
      )
    )
  }
  FALSE
}

is_continupus <- function(x) {
  if ( is.factor(x) | is.character(x) | is_dichotomous(x) )
  {
    return(FALSE)
  }
  else if (is.numeric(x))
  {
    return(TRUE)
  }
  else
  {
    warning(
      paste(
        "The variable is neither categorical nor continuous."
      )
    )
  }
}
