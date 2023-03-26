xx <- mydata %>%
  left_join(divdata) %>%
  filter(variable == "genres") %>%
  group_by(id) %>%
  mutate(
    log_adawareness = log(1 + adawareness),
    log_adawareness0 = log_adawareness - mean(log_adawareness),

    logit_adawareness = logit(adawareness / 100),
    logit_adawareness0 = logit_adawareness - mean(logit_adawareness),

    log_adawareness_lag = lag(log_adawareness),
    log_adawareness0_lag = lag(log_adawareness0),

    logit_adawareness_lag = lag(logit_adawareness),
    logit_adawareness0_lag = lag(logit_adawareness0),

    log_adspend = log1p(adspend) /100,
    log_clutter = log1p(clutter) /100
    ) %>%
  ungroup()
  xx %>% summary

fml_log <- log_adawareness ~  1 + log_adawareness_lag +
  log_clutter + log_adspend
fml_log0 <- log_adawareness0 ~  1 + log_adawareness0_lag +
  log_clutter + log_adspend

fml_logit <- logit_adawareness ~ 1 + logit_adawareness_lag +
  log_clutter + log_adspend
fml_logit0 <- logit_adawareness0 ~ 1 + logit_adawareness0_lag +
  log_clutter + log_adspend
#
# mod <- stan_glm(formula = fml, data = xx)
# mod$coefficients

mod_sum <- stan_glm(formula = fml_logit0, data = xx %>% group_by(id) %>% filter(id == "id_001")) %>% summary()
mod_sum



ind_mod <- xx %>%
  split(.$id) %>%
  map_dfr(
    ~ stan_glm(
      formula = fml_logit0,
      data = .x
    ) %>%
      broom.mixed::tidyMCMC() %>%
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


brms_fml_logit0 <- bf(logit_adawareness0 ~ 0 + f1 + f2, nl = TRUE, center = FALSE) +
  nlf(
    f1 ~ 0 + pgamma + pdelta * logit_adawareness0 - pgamma * pdelta,
    pgamma ~ 1 + 1 | id,
    pdelta ~ 1 + 1 | id
  ) +
  lf(
    f2 ~ 0 + log_clutter + log_adspend + (0 + log_adspend | id),
    center = TRUE
  )

library(parallel)
nchains <- 2
ncores <- 2
nthreads <- detectCores()/nchains


brm_mod <- brms::brm(
  formula = brms_fml_logit0,
  data = xx,
  backend = "cmdstanr",
  threads = ncores,
  cores = ncores,
  chains = nchains,
  prior = c(
    prior(normal(0, 0.5), class = "b", nlpar = "pgamma"),
    prior(normal(0, 0.5), class = "sd", nlpar = "pgamma"),
    prior(normal(0, 0.25), class = "b", nlpar = "pdelta"),
    prior(normal(0, 0.5), class = "sd", nlpar = "pdelta"),
    prior(normal(0, 1), class = "b", nlpar = "f2"),
    prior(normal(0, 0.5), class = "sd", nlpar = "f2"),
    prior(normal(0, 0.1), class = "sigma")
    )
  )



devtools::load_all()
devtools::document()
devtools::install()
devtools::build()
