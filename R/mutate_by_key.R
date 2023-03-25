#' Mutate by key only for tsibble
#'
#' @param .data
#' @param .and_by
#' @param ...
#' @param .keep
#' @param .before
#' @param .after
#'
#' @return
#' @export
#'
#' @examples

mutate_by_key <- function(.data, .and_by = NULL, ... ,
                          .keep = c("all", "used", "unused", "none"),
                          .before = NULL, .after = NULL)
{
  stopifnot(".data must be a tsibble" = tsibble::is_tsibble(.data))

  tbl_data = tidyr::as_tibble(.data)
  keys <- tsibble::key_vars(.data)
  indx <- tsibble::index_var(.data)
  grps <- names(
    tidyselect::eval_select(rlang::expr( c( keys, {{.and_by}}) ) , tbl_data)
  )

  newdata <- dplyr::grouped_df(
    data = tidyr::as_tibble(.data),
    vars = grps
  )

  tsibble::as_tsibble(
    dplyr::mutate(.data = newdata, ... , .keep = {{.keep}}, .before = {{.before}}, .after = {{.after}} ),
    key = keys,
    index = indx
  ) |> ungroup()
}
