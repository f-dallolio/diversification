build_summary <- function(.data,.and_by = NULL, .cols, .fns){
  `%>%` <- dplyr::`%>%`
  stopifnot(".data must be a tsibble" = tsibble::is_tsibble(.data))
  tbl_data = tidyr::as_tibble(.data)
  keys <- tsibble::key_vars(.data)
  grps <- tbl_data %>% select(all_of(keys), {{.and_by}}) %>% names()
  newdata <- dplyr::grouped_df(
    data = tbl_data %>% select(all_of(grps), {{.cols}} ),
    vars = grps
  )

  newdata
}

library(tidyverse)
.data=tsibbledata::global_economy


.fns <- rlang::dots_list(mean=mean, sd=~sd(.x), quantile, .named = TRUE)

i=3
for(i in seq_along(.fns)){
  fx <- purrr::as_mapper(.fns[[i]])

  x = newdata$Imports
  list(fx(x[!is.na(x)])) %>% set_names(names(.fns)[[i]])
}


newdata <-.data %>%
  mutate(pre1975 = Year<1975) %>%
  build_summary(.and_by = pre1975, .cols = c(Imports, Exports), .fns = .fns)
gdata <- map2(.x = newdata$fun_id, .y = newdata$data, .f = ~ do.call())

expand_grid(gdata, funs = .funs)

