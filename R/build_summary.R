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
  group_class <- group_data %>%
    select(-row) %>%
    map_chr(~ class(.x))


  var_data <- tbl_data %>%
    select( row,  {{.cols}} )

  var_names <- var_data %>% select(-row) %>% names
  var_class <- var_data %>%
    select(-row) %>%
    map_chr(~ class(.x))


  fun_list <- dots_list(.fns, .named = TRUE)


  list(
    ts_list = list(data = ts_data),
    group_list = list(names = group_names, class = group_class),
    var_list = list(names = var_names, class = var_class),
    fun_list = .fns )

}
