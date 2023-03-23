#' Get the diva variables out
#'
#' @param .dm
#' @param ...
#' @param .f
#' @param .var
#'
#' @return
#' @export
#'
#' @examples
get_dm_var <- function(.dm, ... , .f = mean, .var){
  f <- as_mapper(.f)
  dots <- enquos(..., .named = TRUE)
  dots_names <- names(dots)
  map_dfr(dots_names,
          ~ dm_all_keys %>% pull_tbl(!!dots_names[[1]]) %>%
            as_tibble()
          ) %>%
    group_by(id, t) %>%
    summarise({{.var}} := f( {{.var}} ) ) %>%
    pull({{.var}})
}
#' @export
dm_num <- function(.dm, ... , .f) get_dm_var(.dm, ..., .f, .var = num)
#' @export
dm_hhi <- function(.dm, ... , .f) get_dm_var(.dm, ..., .f, .var = hhi)
#' @export
dm_ssd <- function(.dm, ... , .f) get_dm_var(.dm, ..., .f, .var = ssd)
#' @export
dm_nef <- function(.dm, ... , .f) get_dm_var(.dm, ..., .f, .var = nef)
#' @export
dm_nfx <- function(.dm, ... , .f) get_dm_var(.dm, ..., .f, .var = nfx)
#' @export
dm_cfx <- function(.dm, ... , .f) get_dm_var(.dm, ..., .f, .var = cfx)
#' @export
dm_tau <- function(.dm, ... , .f) get_dm_var(.dm, ..., .f, .var = tau)
