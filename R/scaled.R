#' Scaling
#'
#' @param x
#' @param center
#' @param scale
#'
#' @return
#' @export
#'
#' @examples
z_scaled <- function(x, center = TRUE, scale = TRUE, na.rm =TRUE) {
  if(center){
    out1 <- x - mean(x, na.rm = na.rm)
  } else {
    out1 <- x
  }
  if(scale){
    out2 <-  sd(x, na.rm = na.rm)
  } else {
    out2 <- 1
  }
  out1/out2
}

#' @export
m_scaled <- function(x, center = TRUE, scale = TRUE, na.rm =TRUE) {
  if(center){
    out1 <- x - median(x, na.rm = na.rm)
  } else {
    out1 <- x
  }
  if(scale){
    out2 <-  IQR(x, na.rm = na.rm)
  } else {
    out2 <- 1
  }
  out1/out2
}

#' @export
scaled <- function(x, center = 0, scale = 1, na.rm = TRUE) {
  if(na.rm){
    x <- x[!is.na(x)]
  }
  out1 <- x - center
  out1/scale
}
