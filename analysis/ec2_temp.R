
# remotes::install_github("f-dallolio/fdutils")
# install.packages("DiagrammeR")
# remotes::install_github("f-dallolio/diversification")
# remotes::install_github("f-dallolio/myloadr")

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
  brms
  # rstanarm,
  # update = FALSE
)

# load("https://www.dropbox.com/s/af2ihecq9cld0hx/dm_divdata.rda?dl=1")
# load("https://www.dropbox.com/s/v4lkjvgxb2q86yx/dm_mydata.rda?dl=1")
load("~/R_wd/r_packages/data_ec2/dm_divdata.rda")
load("~/R_wd/r_packages/data_ec2/dm_mydata.rda")

mydata <- dm_mydata %>%
  pull_tbl(data_df) %>%
  select(
    id, t, category,
    adawareness, impression, consideration, intention,
    adspend, catspend, clutter
  )

summary_select <- mydata %>%
  group_by(id, category) %>%
  summarise(
    N = n(),
    pct_ad1 = mean(adspend > 0),
    max_cons_ad0 = max(consecutive(adspend == 0)),
    mean_adspend = mean(adspend),
    sd_adspend = sd(adspend)
  ) %>%
  ungroup() %>%
  filter(
    N == max(N),
    max_cons_ad0 <= 26
  )

model_data <- mydata %>%
  filter(id %in% summary_select$id)

div_names <- c("dayparts", "genres", "networks")

divdata <- map_dfr(
  .x = div_names,
  .f = ~ dm_divdata %>%
    pull_tbl(!!.x) %>%
    filter(id %in% pull(summary_select, id))
) %>%
  transmute(
    id,
    t,
    variable,
    hhi,
    nfx,
    cfx,
    dfx = 1 - cfx
  )

xx <- mydata %>%
  left_join(divdata) %>%
  filter(variable == "dayparts") %>%
  mutate(dhi = 1-hhi) %>%
  group_by(id) %>%
  mutate(
    log_adawareness = log(1 + adawareness),
    log_adawareness0 = log_adawareness - mean(log_adawareness),
    log_impression = log(1 + impression),
    log_impression0 = log_impression - mean(log_impression),
    log_consideration = log(1 + consideration),
    log_consideration0 = log_consideration - mean(log_consideration),
    log_intention = log(1 + intention),
    log_intention0 = log_intention - mean(log_intention),

    logit_adawareness = logit(adawareness / 100),
    logit_adawareness0 = logit_adawareness - mean(logit_adawareness),
    logit_impression = logit(impression / 100),
    logit_impression0 = logit_impression - mean(logit_impression),
    logit_consideration = logit(consideration / 100),
    logit_consideration0 = logit_consideration - mean(logit_consideration),
    logit_intention = logit(intention / 100),
    logit_intention0 = logit_intention - mean(logit_intention),

    lag_log_adawareness = lag(log_adawareness),
    lag_log_adawareness0 = lag(log_adawareness0),
    lag_log_consideration = lag(log_consideration),
    lag_log_consideration0 = lag(log_consideration0),
    lag_log_impression = lag(log_impression),
    lag_log_impression0 = lag(log_impression0),
    lag_log_intention = lag(log_intention),
    lag_log_intention0 = lag(log_intention0),

    lag_logit_adawareness = lag(logit_adawareness),
    lag_logit_adawareness0 = lag(logit_adawareness0),
    lag_logit_impression = lag(logit_impression),
    lag_logit_impression0 = lag(logit_impression0),
    lag_logit_consideration = lag(logit_consideration),
    lag_logit_consideration0 = lag(logit_consideration0),
    lag_logit_intention = lag(logit_intention),
    lag_logit_intention0 = lag(logit_intention0),

    log_adspend = log1p(adspend) /100,
    log_clutter = log1p(clutter) /100
  ) %>%
  ungroup()
xx %>% summary

fml_log <- log_adawareness ~  1 + lag_log_adawareness +
  log_clutter + log_adspend +
  log_adspend:dhi
fml_log0 <- log_adawareness0 ~  1 + lag_log_adawareness0 +
  log_clutter + log_adspend +
  log_adspend:dhi

fml_logit <- logit_adawareness ~ 1 + lag_logit_adawareness +
  log_clutter + log_adspend +
  log_adspend:dhi
fml_logit0 <- logit_adawareness0 ~ 1 + lag_logit_adawareness0 +
  log_clutter + log_adspend +
  log_adspend:dhi
#
# mod <- stan_glm(formula = fml, data = xx)
# mod$coefficients

# mod_sum <- stan_glm(formula = fml_logit0, data = xx %>% group_by(id) %>% filter(id == "id_001")) %>% summary()
# mod_sum



ind_mod <- xx %>%
  split(.$id) %>%
  map_dfr(
    ~ lm(
      formula = fml_log0,
      data = .x
    ) %>%
      tidy %>%
      as_tibble %>%
      mutate(id = unique(.x$id), .before = 1 ) )

split_output <- ind_mod %>%
  mutate(term = if_else(term =="(Intercept)", "Intercept", term)) %>%
  pivot_longer(c(estimate, std.error)) %>%
  split(.$name)

out_mu <- split_output$estimate %>%
  group_by(term) %>%
  summarise(mu_mean = mean(value),
            mu_stdev = sd(value),
            mu_stdev05 = mu_stdev/2,
            pnorm(mu_stdev05, 0, .5))
out_mu

out_sd <- split_output$std.error %>%
  group_by(term) %>%
  summarise(sd_mean = mean(value),
            sd_stdev = sd(value),
            sd_stdev05 = sd_stdev/2,
            pnorm(sd_stdev05, 0, .5))
out_sd


aaw_log0 <- bf(log_adawareness0 ~ 0 +
                 pgamma +
                 inv_logit(pdelta) * (lag_log_adawareness0 - pgamma) +
                 f2,
               pgamma ~ 1 + 1 | id,
               pdelta ~ 1 + 1 | id,
               nl = TRUE,
               center = TRUE) +
  lf(f2 ~ 0 + log_clutter +
       log_adspend +
       log_adspend : dhi + (0 + log_adspend | id))

imp_log0 <- bf(log_impression0 ~ 0 +
                 pgamma +
                 inv_logit(pdelta) * (lag_log_impression0 - pgamma) +
                 f2,
               pgamma ~ 1 + 1 | id,
               pdelta ~ 1 + 1 | id,
               nl = TRUE,
               center = TRUE) +
  lf(f2 ~ 0 +
       lag_log_adawareness0 +
       log_clutter +
       log_adspend +
       log_adspend : dhi + (0 + log_adspend | id))

con_log0 <- bf(log_consideration0 ~ 0 +
                 pgamma +
                 inv_logit(pdelta) * (lag_log_consideration0 - pgamma) +
                 f2,
               pgamma ~ 1 + 1 | id,
               pdelta ~ 1 + 1 | id,
               nl = TRUE,
               center = TRUE) +
  lf(f2 ~ 0 +
       lag_log_impression0 +
       lag_log_adawareness0 +
       log_clutter +
       log_adspend +
       log_adspend : dhi + (0 + log_adspend | id))

int_log0 <- bf(log_intention0 ~ 0 +
                 pgamma +
                 inv_logit(pdelta) * (lag_log_intention0 - pgamma) +
                 f2,
               pgamma ~ 1 + 1 | id,
               pdelta ~ 1 + 1 | id,
               nl = TRUE,
               center = TRUE) +
  lf(f2 ~ 0 +
       lag_log_consideration0 +
       lag_log_impression0 +
       lag_log_adawareness0 +
       log_clutter +
       log_adspend +
       log_adspend : dhi + (0 + log_adspend | id))



library(parallel)
nchains <- 2
ncores <- 2
nthreads <- detectCores()/nchains



t0 <- lubridate::now()
print(t0)
aaw_log0_mod <- brms::brm(
  formula = aaw_log0,
  data = xx,

  backend = "cmdstanr",
  threads = nthreads,
  cores = ncores,
  chains = nchains,
  init = 0,
  refresh = 25,
  prior = c(
    prior(normal(0, 1), class = "b", nlpar = "pgamma"),
    prior(normal(0, 1), class = "sd", nlpar = "pgamma"),
    prior(normal(0, 1), class = "b", nlpar = "pdelta"),
    prior(normal(0, 1), class = "sd", nlpar = "pdelta"),
    prior(normal(0, 1), class = "b", nlpar = "f2"),
    prior(normal(0, 1), class = "sd", nlpar = "f2"),
    prior(normal(0, 0.5), class = "sigma")
  )
)

t1 <- lubridate::now()
print(t1-t0)
aaw_log0_mod
# saveRDS(object = aaw_log0_mod, file = "~/Dropbox/projects/Data_packages/aaw_log0_mod_dayparts.rds")
saveRDS(object = aaw_log0_mod, file = "~/R_wd/r_packages/results_ec2/aaw_log0_mod_dayparts.rds")

t0 <- lubridate::now()
print(t0)
imp_log0_mod <- brms::brm(
  formula = imp_log0,
  data = xx,

  backend = "cmdstanr",
  threads = nthreads,
  cores = ncores,
  chains = nchains,
  init = 0,
  refresh = 25,
  prior = c(
    prior(normal(0, 1), class = "b", nlpar = "pgamma"),
    prior(normal(0, 1), class = "sd", nlpar = "pgamma"),
    prior(normal(0, 1), class = "b", nlpar = "pdelta"),
    prior(normal(0, 1), class = "sd", nlpar = "pdelta"),
    prior(normal(0, 1), class = "b", nlpar = "f2"),
    prior(normal(0, 1), class = "sd", nlpar = "f2"),
    prior(normal(0, 0.5), class = "sigma")
  )
)

t1 <- lubridate::now()
print(t1-t0)
imp_log0_mod
# saveRDS(object = imp_log0_mod, file = "~/Dropbox/projects/Data_packages/imp_log0_mod_dayparts.rds")
saveRDS(object = aaw_log0_mod, file = "~/R_wd/r_packages/results_ec2/imp_log0_mod_dayparts.rds")

t0 <- lubridate::now()
print(t0)
con_log0_mod <- brms::brm(
  formula = con_log0,
  data = xx,

  backend = "cmdstanr",
  threads = nthreads,
  cores = ncores,
  chains = nchains,
  init = 0,
  refresh = 25,
  prior = c(
    prior(normal(0, 1), class = "b", nlpar = "pgamma"),
    prior(normal(0, 1), class = "sd", nlpar = "pgamma"),
    prior(normal(0, 1), class = "b", nlpar = "pdelta"),
    prior(normal(0, 1), class = "sd", nlpar = "pdelta"),
    prior(normal(0, 1), class = "b", nlpar = "f2"),
    prior(normal(0, 1), class = "sd", nlpar = "f2"),
    prior(normal(0, 0.5), class = "sigma")
  )
)

t1 <- lubridate::now()
print(t1-t0)
con_log0_mod
# saveRDS(con_log0_mod, "~/Dropbox/projects/Data_packages/con_log0_mod_dayparts.rds")
saveRDS(object = aaw_log0_mod, file = "~/R_wd/r_packages/results_ec2/con_log0_mod_dayparts.rds")

t0 <- lubridate::now()
print(t0)
int_log0_mod <- brms::brm(
  formula = int_log0,
  data = xx,

  backend = "cmdstanr",
  threads = nthreads,
  cores = ncores,
  chains = nchains,
  init = 0,
  refresh = 25,
  prior = c(
    prior(normal(0, 1), class = "b", nlpar = "pgamma"),
    prior(normal(0, 1), class = "sd", nlpar = "pgamma"),
    prior(normal(0, 1), class = "b", nlpar = "pdelta"),
    prior(normal(0, 1), class = "sd", nlpar = "pdelta"),
    prior(normal(0, 1), class = "b", nlpar = "f2"),
    prior(normal(0, 1), class = "sd", nlpar = "f2"),
    prior(normal(0, 0.5), class = "sigma")
  )
)

t1 <- lubridate::now()
print(t1-t0)
int_log0_mod
# saveRDS(int_log0_mod, "~/Dropbox/projects/Data_packages/int_log0_mod_dayparts.rds")
saveRDS(object = aaw_log0_mod, file = "~/R_wd/r_packages/results_ec2/int_log0_mod_dayparts.rds")


#
# devtools::load_all()
# devtools::document()
# devtools::install()
# devtools::build()

