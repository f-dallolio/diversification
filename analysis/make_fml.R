library(readxl)

remotes::install_github("f-dallolio/fdutils")
remotes::install_github("f-dallolio/diversification")
remotes::install_github("f-dallolio/myloadr")

myloadr::myloadr(
  rlang,
  tidyverse,
  tsibble,
  broom,
  lubridate,
  ggplot2,
  glue,
  skimr,
  fdutils,
  diversification,
  dm,
  cmdstanr,
  brms,
  rstanarm,
  update = FALSE
)

library(parallel)
nchains <- 2
ncores <- 2
nthreads <- floor(detectCores()/nchains)


load("~/Dropbox/projects/Data_packages/dm_divdata.rda")
load("~/Dropbox/projects/Data_packages/dm_mydata.rda")

mydata <- dm_mydata %>%
  pull_tbl(data_df) %>%
  transmute(
    id, t, category,
    ln_aaw = log(adawareness),
    ln_imp = log(impression),
    ln_con = log(consideration),
    ln_int = log(intention),

    ln_clutter = log1p(clutter) / 100,
    ln_adspend = log1p(adspend) / 100
  )




div_names <- c("dayparts", "genres", "networks")
divdata0 <- map_dfr(
  .x = div_names,
  .f = ~ dm_divdata %>%
    pull_tbl(!!.x)
) %>%
  left_join(
    mydata %>% select( id, t, ln_adspend ) ) %>%
  transmute(
    id, t, variable,
    ln_adspend_x_dhi = ln_adspend * (1 - hhi)
  )

divdata_gnd <- divdata0 %>%
  mutate(
    variable = str_c("ln_adspend_x_dhi_", variable),
    variable = str_replace_all(variable, "dayparts","d") %>%
      str_replace_all("genres","g") %>%
      str_replace_all("networks","n")
  ) %>%
  pivot_wider(names_from = variable,
              values_from = ln_adspend_x_dhi)

divdata_ww <- divdata0 %>%
  mutate( variable_ww = if_else(variable == "dayparts", "wn", "wr") ) %>%
  group_by( id, t, variable_ww ) %>%
  summarise( value = mean(ln_adspend_x_dhi) ) %>%
  mutate( variable_ww = str_c("ln_adspend_x_dhi_", variable_ww) ) %>%
  pivot_wider(names_from = variable_ww)

divdata_all<- divdata0 %>%
  group_by( id, t ) %>%
  summarise( ln_adspend_x_dhi_all = mean(ln_adspend_x_dhi) )

divdata <- divdata_gnd %>%
  left_join(divdata_ww) %>%
  left_join(divdata_all)


cat_data <- dm_mydata %>%
  pull_tbl(cat_moderators) %>%
  select( category,  hedonic, risk, involvement )

brd_data <- dm_mydata %>%
  pull_tbl(brand_moderators) %>%
  select( id, size )


summary_select <- mydata %>%
  group_by(id, category) %>%
  summarise(
    N = n(),
    pct_ad1 = mean(expm1(ln_adspend) > 0),
    max_cons_ad0 = max(consecutive(expm1(ln_adspend) == 0)),
    mean_adspend = mean(expm1(ln_adspend)),
    sd_adspend = sd(expm1(ln_adspend))
  ) %>%
  ungroup() %>%
  filter(
    N == max(N),
    max_cons_ad0 <= 26
  )




model_data <- mydata %>%
  left_join(divdata) %>%
  left_join(cat_data) %>%
  left_join(brd_data) %>%
  filter(id %in% summary_select$id)



model_data




type <- list(dn = divdata %>% select(ends_with(c("_d", "_n"))) %>% names ,
             dg = divdata %>% select(ends_with(c("_d", "_g"))) %>% names ,
             ww = divdata %>% select(ends_with(c("_wn", "_wr"))) %>% names ,
             all = divdata %>% select(contains("_all")) %>% names )
map(type, ~ paste0(.x, collapse = " + "))

unlist(map(type,~glue("()")))




div=unlist(map(type, ~glue({glue_collapse(.x, ' + ')}) %>% embrace_with()))
mod <- names(brd_data)[-1] %>% c(names(cat_data)[-1])
dv <- c("ln_aaw", 'ln_imp', "ln_con", "ln_int")
ctr <- "ln_clutter"
ad <- "ln_adspend"
hier <- c(NA,  map_chr(.x = seq_along(dv)[-1],
            .f = ~ glue_collapse( glue("lag({dv[(.x-1) : 1]})"), " + ")))
lag_dv <- glue("b0 + ar0(lag({dv}) - b0)")

tb1 <- tibble(dv, lag_dv, hier = hier, ctr = ctr, ad = ad)

xx <- expand_grid(div = div, mod = mod, dv = dv) %>%
  left_join(tb1) %>%
  mutate(row = row_number())

f_ar <- function(x, b0_gr = ~ 1|id, ar_gr = ~ 1|id ){
  .x <-  enquos(1, .named = TRUE) %>% names
  b0_fml <- update.formula(b0_gr, b0 ~ .)
  ar_fml <- update.formula(ar_gr, ar ~ .)

  fml <-  glue("b0 + inv_logit(ar) * (lag({.x})  - b0)")
  return(fml)
  f_nl <- nlf(
    formula =brmsformula( eval(call(glue(fml)))),
    b0_fml,
    ar_fml
  )
  # return(f_nl)
}

get_prior(f_ar, model_data)


f_ar(ln_aaw)

embrace_with <- function(.x, embrace = c("(",")")){
  paste0(
    embrace[1],
    .x,
    embrace[2]
  )
}


fml <- ~ lag(dv) + hier + ctrl + ad/mod + div/mod



ff <- attr(terms(fml), "term.labels") %>%
  embrace_with() %>%
  paste0(. ,collapse = " + ") %>%
  str_c("~ ", .) %>%
  as.formula()

xx %>% model.frame(ff, data = .)





