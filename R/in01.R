#' Between 0 and 1
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
logit <- function(x, ...){
  x <- x[!is.na(x)]
  qlogis(x)
}

#' @export
inv_logit <- function(x){
  x <- x[!is.na(x)]
  plogis(x)
}

#' @export
minmax <- function(x, min, max, alpha = 0) {
  alpha1 = 1 - (alpha/2)
  x <- x[!is.na(x)]
  med <- median(x)
  out <-  (((x - med) * alpha1) + med) / (max - min)
  return(out)
}
