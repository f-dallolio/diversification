#' Summary Tables for Tsibble
#'
#' @param .data
#' @param .and_by
#' @param .cols
#' @param .fns
#'
#' @return
#' @export
#'
#' @examples
build_summary <- function(.data, .and_by, .cols, .fns){

  stopifnot(".data must be a tsibble" = tsibble::is_tsibble(.data))

  keys <- key_vars(.data)
  idx <- index_var(.data)
  ts_attributes <- attributes(.data)

  tbl_data = as_tibble(.data)

  group_data <- tbl_data %>%
    transmute(
      across(
        c(all_of(keys), {{.and_by}} ),
        as_factor)
      ) %>%
    grouped_df(names(.))
  group_attr <- map(group_data, ~ list(class = class(.x), attributes = attributes(.x))) %>%
    set_names(names(group_data))

  var_data <- tbl_data %>%
    select( {{.cols}} )
  var_attr <- map(var_data, ~ list(class = class(.x), attributes = attributes(.x)))%>%
    set_names(names(var_data))

  fun_list <- dots_list(.fns, .named = TRUE)

  list(
    ts_attributes = ts_attributes,
    group_list = c(list(data = group_data),
                   list(attributes = group_attr)),
    var_list = c(list(data = var_data),
                 list(attributes = var_attr)),
    fun_list = .fns )

}
