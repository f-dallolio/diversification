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




tbl <- read_excel("~/Dropbox/projects/ad_diversification/shared/Ad Diversification/Results/2023_03_28/2023_03_28_results.xlsx")

dv <- c("ln_aaw", "ln_imp", "ln_con", "ln_int")
lag_dv <- str_c("lag_", dv)


glue_fml_aaw <-
" bf( { dv[1]} ~
0 + b0 + inv_logit(dd) * (lag_ln_aaw - b0) + f2,
b0 ~ 1 + 1 | id,
dd ~ 1 + 1 | id,
nl = TRUE, center = TRUE )" +
"lf(f2 ~ 0 + ln_clutter + ln_adspend +
  (0 + ln_clutter + ln_adspend | id),
  center = TRUE)"


glue_fml_imp <-
" bf( { dv[2]} ~
0 + b0 + inv_logit(dd) * (lag_ln_aaw - b0) + f2,
b0 ~ 1 + 1 | id,
dd ~ 1 + 1 | id,
nl = TRUE, center = TRUE ) +
lf(f2 ~ 0 + lag_ln_aaw + ln_clutter + ln_adspend +
  (0 + lag_ln_aaw + ln_clutter + ln_adspend | id),
  center = TRUE)"


glue_fml_con <-
" bf( { dv[3]} ~
0 + b0 + inv_logit(dd) * (lag_ln_aaw - b0) + f2,
b0 ~ 1 + 1 | id,
dd ~ 1 + 1 | id,
nl = TRUE, center = TRUE ) +
lf(f2 ~ 0 + lag_ln_con + lag_ln_aaw + ln_clutter + ln_adspend +
  (0 + lag_ln_con + lag_ln_aaw + ln_clutter + ln_adspend | id),
  center = TRUE)"


glue_fml_int <-
" bf( { dv[4]} ~
0 + b0 + inv_logit(dd) * (lag_ln_aaw - b0) + f2,
b0 ~ 1 + 1 | id,
dd ~ 1 + 1 | id,
nl = TRUE, center = TRUE ) +
lf(f2 ~ 0 + lag_ln_inp + lag_ln_con + lag_ln_aaw + ln_clutter + ln_adspend +
  (0 + lag_ln_inp + lag_ln_con + lag_ln_aaw + ln_clutter + ln_adspend | id),
  center = TRUE)"

library(brms)
library(parallel)

nchains <- ncores <- 2
nthreads <- floor(detectCores() / nchains)


fml_aaw <- bf(
  ln_aaw ~ 0 + b0 + inv_logit(dd) * (lag_ln_aaw - b0) + f2,
  b0 ~ 1 + (1 | id),
  dd ~ 1 + (1 | id),
  nl = TRUE,
  center = TRUE ) +
  lf(
    f2 ~ 0 + ln_clutter + ln_adspend +
      ( 0 + ln_clutter + ln_adspend | id ),
    center = TRUE
  )

priors <-  c(
  prior(normal(0, 1), class = "b", nlpar = "b0"),
  prior(normal(0, 1), class = "sd", nlpar = "b0"),
  prior(normal(0, 1), class = "b", nlpar = "dd"),
  prior(normal(0, 1), class = "sd", nlpar = "dd"),
  prior(normal(0, 1), class = "b", nlpar = "f2"),
  prior(normal(0, 1), class = "sd", nlpar = "f2"),
  prior(normal(0, 0.5), class = "sigma")
)

fit_aaw <- brm(
  formula = fml_aaw,
  data = model_data %>%
    group_by(id) %>%
    mutate( across( contains( dv ), list(lag = ~ lag(.x)), .names = "{.fn}_{.col}" ) ) %>%
    ungroup(),
  backend = "cmdstan",
  prior = priors,
  threads = nthreads,
  chains = nchains,
  cores = ncores,
  init = 0,
  refresh = 25
)






fml_aaw <- eval(
  str2expression(
    glue(glue_fml_aaw)
  )
)

fml_imp <- eval(
  str2expression(
    glue(glue_fml_imp)
  )
)

fml_con <- eval(
  str2expression(
    glue(glue_fml_con)
  )
)
fml_int <- eval(
  str2expression(
    glue(glue_fml_int)
  )
)

