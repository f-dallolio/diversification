#' Create concentration/diversification measures
#'
#' @param .data the data frame with the data.
#' @param .fn the function used. Must be one of `c("cfx", "nfx", "tau", "dfx", "dau")`.
#' @param .var the variable being transformed. Must be one of ...
#' @param .id ...
#'
#' @return a numeric vector.
#' @export
make_div_fn <- function(.data = panel_df, .fn, .var, .id = 1) {
  stopifnot(
    ".fn must be one of c(`cfx`, `nfx`, `tau`, `dfx`, `dau`)" =
      .fn %in% c("cfx", "nfx", "tau", "dfx", "dau")
  )

  lup <- attr(.data, "div_lookup") %>%
    filter(
      variable == .var,
      id == .id
    ) %>%
    select(num, hhi, sd) %>%
    as.list()

  .num <- pluck(.data, lup[["num"]])
  .hhi <- pluck(.data, lup[["hhi"]])
  .sd <- pluck(.data, lup[["sd"]])

  if (.fn == "cfx") {
    out <- make_cfx(num = .num, hhi = .hhi)
  }
  if (.fn == "nfx") {
    out <- make_nfx(num = .num, hhi = .hhi)
  }
  if (.fn == "tau") {
    out <- make_tau(num = .num, hhi = .hhi)
  }
  if (.fn == "dfx") {
    out <- 1 - make_cfx(num = .num, hhi = .hhi)
  }
  if (.fn == "dau") {
    out <- 1 - make_tau(num = .num, hhi = .hhi)
  }
  attr(out, "var_name") <- str_c(.var, "_", .fn)

  return(out)
}

#' @export
cfx <- function(.var, ...) {
  make_div_fn(.data, .fn = "cfx", .var = .var)
}

#' @export
nfx <- function(.var, ...) {
  make_div_fn(.data, .fn = "nfx", .var = .var)
}

#' @export
tau <- function(.var, ...) {
  make_div_fn(.data, .fn = "tau", .var = .var)
}

#' @export
dfx <- function(.var, ...) {
  make_div_fn(.data, .fn = "dfx", .var = .var)
}

#' @export
dau <- function(.var, ...) {
  make_div_fn(.data, .fn = "dau", .var = .var)
}
