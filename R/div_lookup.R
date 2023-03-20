#' Lookup table for concentration/diversification variables
#'
#' @param x tibble with the data
#' @param nms names used to select the concentration/diversification variables
#'
#' @return a tibble
#' @export
#'
div_lookup <- function(
    x,
    nms = c("_num", "_hhi")
)
{
  x <- as_tibble( x )

  x_nms <- x %>%
    select( contains( nms ) ) %>%
    names()

  nms_split <- x_nms %>%
    str_split( pattern = "_" )

  x_nms %>%
    str_split( pattern = "_" ) %>%
    map_dfr(
      .f = ~ .x %>%
        as_tibble() %>%
        mutate(
          name = str_c("V", seq_along(.x))
        ) %>%
        pivot_wider()
    ) %>%
    rename(
      variable = V1,
      type = V2,
      id = V3
    ) %>%
    mutate(
      id = as.numeric(id),
      id = case_when(is.na(id) ~ 1, .default = id),
      var0 = x_nms
    ) %>%
    relocate(
      id,
      .after = variable
    ) %>%
    arrange(
      variable,
      id
    ) %>%
    mutate(
      across(
        .cols = !var0,
        .fns = ~ as_factor(.x)
      )
    ) %>%
    pivot_wider(
      names_from = type,
      values_from = var0
    )
}
