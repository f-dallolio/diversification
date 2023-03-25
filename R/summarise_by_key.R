#' Summarse by key only for tsibble
#'
#' @param .data
#' @param ...
#' @param .and_by
#' @param .groups
#'
#' @return
#' @export
#' @import tsibble
#' @examples
summarise_by_key <- function(.data, .and_by = NULL, ... , .groups = NULL){
  stopifnot(".data must be a tsibble" = tsibble::is_tsibble(.data))
  tbl_data = tidyr::as_tibble(.data)
  keys <- key_vars(.data)
  grps <- names(
    tidyselect::eval_select(rlang::expr( c( keys, {{.and_by}}) ) , tbl_data)
  )
  newdata <- dplyr::grouped_df(
    data = tidyr::as_tibble(.data),
    vars = grps
  )
  dplyr::summarise(.data = newdata, ... , .by = NULL)
}

#' @export
reframe_by_key <- function(.data, ... , .and_by = NULL, .groups = NULL){
  stopifnot(".data must be a tsibble" = tsibble::is_tsibble(.data))
  tbl_data = tidyr::as_tibble(.data)
  keys <- key_vars(.data)
  grps <- names(
    tidyselect::eval_select(rlang::expr( c( keys, {{.and_by}}) ) , tbl_data)
  )
  newdata <- dplyr::grouped_df(
    data = tidyr::as_tibble(.data),
    vars = grps
  )
  dplyr::reframe(.data = newdata, ... , .by = NULL)
}
