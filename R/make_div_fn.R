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

  # .var = enquo( .var )

  stopifnot(
    ".fn must be one of c('num', 'hhi', `cfx`, `nfx`, `tau`, `dfx`, `dau`)" =
      .fn %in% c("num", "hhi", "cfx", "nfx", "tau", "dfx", "dau")
  )
  attr_not_null <- ! is.null( attr(.data, "div_lookup") )

  stopifnot("attr(.data, 'div_lookup') must be different than NULL" = attr_not_null)

  lup <- attr(.data, "div_lookup") %>%
    filter(
      variable == (.var),
      id == .id
    ) %>%
    select(num, hhi) %>%
    as.list()

  .num <- pluck(.data, lup[["num"]])
  .hhi <- pluck(.data, lup[["hhi"]])

  if (.fn == "num") {
    return(.num)
  }
  if (.fn == "hhi") {
    return(.hhi)
  }
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
  attr(out, "var_name") <- str_c( (.var), "_", .fn )

  return(out)
}


#' @export
num <- function(.var, ...) {
  .var = enquo( .var )
  make_div_fn(.fn = "num", .var = as_label(.var) )
}



#' @export
hhi <- function(.var, ...) {
  .var = enquo( .var )
  make_div_fn(.fn = "hhi", .var = as_label(.var))
}

#' @export
cfx <- function(.var, ...) {
  .var = enquo( .var )
  make_div_fn(.fn = "cfx", .var = as_label(.var))
}

#' @export
nfx <- function(.var, ...) {
  .var = enquo( .var )
  make_div_fn(.fn = "nfx", .var = as_label(.var))
}

#' @export
tau <- function(.var, ...) {
  .var = enquo( .var )
  make_div_fn(.fn = "tau", .var = as_label(.var))
}

#' @export
dfx <- function(.var, ...) {
  .var = enquo( .var )
  make_div_fn(.fn = "dfx", .var = as_label(.var))
}

#' @export
dau <- function(.var, ...) {
  .var = enquo( .var )
  make_div_fn(.fn = "dau", .var = as_label(.var))
}

