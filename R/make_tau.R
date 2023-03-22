#' Diversifications Measures
#'
#' @param num a numeric vector.
#' @param hhi a numeric vector.
#'
#' @return a numeric vector.
#' @export
make_tau <- function(num, hhi) {
  hhi <-  hhi * (num > 0)
  old <- tibble(id = seq_along(num), num, hhi)
  new <- old %>%
    filter(num > 1) %>%
    mutate(
      nef = 1/hhi,
      cfx_num = num * hhi - 1,
      cfx_num = case_when(nef >= num ~ 0, .default = cfx_num),
      nef = case_when(nef >= num ~ num, .default = nef),
      cfx_den = num - 1,
      cfx0 = cfx_num / cfx_den,
      cfx = cfx0 * (cfx0 > 0),
      tau = sqrt(cfx),
      nfx = hhi - cfx
    )
  out <- old %>%
    left_join(new, by = "id") %>%
    mutate(
      across(
        everything(),
        ~ if_else(is.na(.x), 0, .x)
      )
    ) %>%
    select(cfx, nfx, tau)
  out$tau
}

#' @export
make_cfx <- function(num, hhi) {
  hhi = hhi * (num > 0)
  old <- tibble(id = seq_along(num), num, hhi)
  new <- old %>%
    filter(num > 1) %>%
    mutate(
      nef = 1/hhi,
      cfx_num = num * hhi - 1,
      cfx_num = case_when(nef >= num ~ 0, .default = cfx_num),
      nef = case_when(nef >= num ~ num, .default = nef),
      cfx_den = num - 1,
      cfx0 = cfx_num / cfx_den,
      cfx = cfx0 * (cfx0 > 0),
      tau = sqrt(cfx),
      nfx = hhi - cfx
    )
  out <- old %>%
    left_join(new, by = "id") %>%
    mutate(
      across(
        everything(),
        ~ if_else(is.na(.x), 0, .x)
      )
    ) %>%
    select(cfx, nfx, tau)
  out$cfx
}

#' @export
make_nfx <- function(num, hhi) {
  hhi = hhi * (num > 0)
  old <- tibble(id = seq_along(num), num, hhi)
  new <- old %>%
    filter(num > 1) %>%
    mutate(
      nef = 1/hhi,
      cfx_num = num * hhi - 1,
      cfx_num = case_when(nef >= num ~ 0, .default = cfx_num),
      nef = case_when(nef >= num ~ num, .default = nef),
      cfx_den = num - 1,
      cfx0 = cfx_num / cfx_den,
      cfx = cfx0 * (cfx0 > 0),
      tau = sqrt(cfx),
      nfx = hhi - cfx
    )
  out <- old %>%
    left_join(new, by = "id") %>%
    mutate(
      across(
        everything(),
        ~ if_else(is.na(.x), 0, .x)
      )
    ) %>%
    select(cfx, nfx, tau)
  out$nfx
}
