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
    var_names = var_names[var_id],
    var_class = var_class[var_id],
    var_type = var_type[var_id]
  )
  fun_tbl <- tibble(
    fun_id = seq_along(fun_list),
    fun_name = names(fun_list)
    )

  look_tbl <- select(.data = groups_tbl, grp_id, row0, row1) %>%
    expand_grid(var_names = var_tbl$var_names) %>%
    expand_grid(fun_id = fun_tbl$fun_id) %>%
    mutate(row_id = row_number(), .before = 1)

  return(
    list(
      look_tbl,
      groups_tbl,
      var_tbl,
      fun_tbl,
      fun_list
    )
  )
}

# remotes::install_github("f-dallolio/diversification")
# remotes::install_github("f-dallolio/fdutils")
# remotes::install_github("f-dallolio/myloadr")
#
# library(myloadr)
# myloadr(
#   tidyverse,
#   tsibble,
#   tsibbledata,
#   rlang,
#   stringr,
#   fdutils,
#   diversification,
#   update = TRUE
# )
#
# ts_data <- global_economy %>%
#   rename_with(.fn = stringr::str_to_lower, .cols = everything()) %>%
#   mutate(
#     y1975 = case_when(year < 1975 ~ "pre1975", .default = "post1975")
#   )
#
# .out_list <- build_summary(.data  = ts_data,
#                     .and_by = y1975,
#                     .cols = c(imports, exports, code),
#                     .fns = list(mean, sd, quantile))
#
# .out_list
#
#
# summary_getter <- function(.data, .out_list){ # grp_id, row0, row1, var_id, fun_id ){
#
#   row_id <- pluck(.out_list, "look_tbl", "row_id")
#   grp_id <- pluck(.out_list, "look_tbl", "grp_id")
#   row0 <- pluck(.out_list, "look_tbl", "row0")
#   row1 <-  pluck(.out_list, "look_tbl", "row1")
#   var_names <-  pluck(.out_list, "look_tbl", "var_names")
#   fun_id <-  pluck(.out_list, "look_tbl", "fun_id")
#   fun_fx <- pluck(.out_list, "fun_list")
#
#   var_data <- .data %>%
#     as_tibble() %>%
#     select(all_of(pluck(.out_list, "look_tbl", "var_names")))
#
#   summary_list <- as.list(seq_along(row_id))
#   i=1
#   for(i in seq_along(row_id)){
#     row_id_i <- row_id[i]
#     grp_id_i <- grp_id[i]
#     row0_i <- row0[i]
#     row1_i <- row1[i]
#     var_names_i <- var_names[i]
#     fun_id_i  <- fun_id[i]
#     fun_fx_i <- fun_fx[i]
#
#     summary_list[[i]] <- var_data %>%
#       filter( between(row_number(), row0_i, row1_i) ) %>%
#       pull(var_names_i)
#     print(i)
#   }
# summary_list
# }
#
# summary_getter(.data = .data, .out_list = out_list)
#
# sum_tbl <- skimr::skim(data = global_economy %>% as_tibble %>% group_by(Country)) %>%
#   as_tibble
