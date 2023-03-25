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

  ts_data = as_tibble(.data) %>%
    mutate(row = row_number()) %>%
    select(row, all_of(idx), all_of(keys), {{.and_by}}, {{.cols}} )

  ts_attributes <- attributes(.data)


  tbl_data = as_tibble(ts_data)


  group_data <- tbl_data %>%
    select(row, all_of(keys), {{.and_by}} ) %>%
    mutate(
      across(
        everything(),
        as_factor)
      ) %>%
    grouped_df(vars = names(.)[-1])

  group_names <- group_data %>% select(-row) %>% names

  group_attr <- group_data %>%
    select(-row) %>%
    map( ~ list(class = class(.x), attributes = attributes(.x))) %>%
    set_names(group_names)


  var_data <- tbl_data %>%
    select( row,  {{.cols}} )

  var_names <- var_data %>% select(-row) %>% names

  var_attr <- var_data %>%
    select(-row) %>%
    map( ~ list(class = class(.x), attributes = attributes(.x))) %>%
    set_names(var_names)

  fun_list <- dots_list(.fns, .named = TRUE)

  list(
    ts_list = c(list(data = ts_data),
                ts_attributes),
    group_list = list(names = group_names,
                      attributes = group_attr),
    var_list = list(names = var_names,
                    attributes = var_attr),
    fun_list = .fns )

}
