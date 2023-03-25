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
    select(all_of(idx), all_of(keys), {{.and_by}}, {{.cols}} ) %>%
    mutate( across( c( all_of(keys), {{.and_by}} ), as_factor) )

  tbl_data = as_tibble(ts_data)

  group_data <- tbl_data %>%
    select( all_of(keys), {{.and_by}} ) %>%
    # select(row, all_of(keys), {{.and_by}} ) %>%
    grouped_df(vars = names(.))
  group_names <- group_data %>%
    # select(-row) %>%
    names()
  group_class <- group_data %>%
    # select(-row) %>%
    map_chr(~ class(.x))


  var_data <- tbl_data %>%
    select( {{.cols}} )
    # select( row,  {{.cols}} )
  var_names <- var_data %>%
    # select(-row) %>%
    names()
  var_class <- var_data %>%
    map_chr(~ class(.x))
  var_type <- var_data %>%
    map_chr(~ case_when(is_categorical(.x) ~ "categprical",
                        is_continupus(.x) ~ "continuous") )

  fun_list <- .fns

  groups_tbl <- ts_data %>%
    mutate(row = row_number()) %>%
    grouped_df(
      vars = group_names
    ) %>%
    summarise(
      grp_id = cur_group_id(),
      row0 = min(row),
      row1 = max(row),
      .groups = "drop"
    ) %>%
    relocate(grp_id, 1)
  var_tbl <- tibble(
    var_id = var_names %>% seq_along,
    var_name = var_names[var_id],
    var_class = var_class[var_id],
    var_type = var_type[var_id]
  )
  fun_tbl <- tibble(
    fun_id = seq_along(fun_list),
    fun_name = names(fun_list)
  )

  out_tbl <- select(.data = groups_tbl, grp_id, row0, row1) %>%
    expand_grid(var_id = var_tbl$var_id) %>%
    expand_grid(fun_id = fun_tbl$fun_id)

  return(
    list(
      out_tbl,
      groups_tbl,
      var_tbl,
      fun_tbl,
    )
  )
}

# remotes::install_github("f-dallolio/diversification")
# remotes::install_github("f-dallolio/fdutils")
# remotes::install_github("f-dallolio/myloadr")
#
library(myloadr)
myloadr(
  tidyverse,
  tsibble,
  tsibbledata,
  rlang,
  stringr,
  fdutils,
  diversification,
  update = TRUE
)

ts_data <- global_economy %>%
  rename_with(.fn = stringr::str_to_lower, .cols = everything()) %>%
  mutate(
    y1975 = case_when(year < 1975 ~ "pre1975", .default = "post1975")
  )

bs <- build_summary(.data = ts_data,
                    .and_by = y1975,
                    .cols = c(imports, exports, code),
                    .fns = list(mean, sd, quantile))
bs
