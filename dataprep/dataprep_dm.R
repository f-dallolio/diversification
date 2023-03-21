library(tidyverse)
library(tsibble)
library(fdutils)
library(dm)

load("data/mydata.rda")

panel_table <- mydata %>%
  select(id, brand : sector) %>%
  mutate(id = as_factor(id)) %>%
  distinct()

time_table <- mydata %>%
  select(t, weeknum, year) %>%
  distinct() %>%
  mutate(yearweek = make_yearweek(year = year, week = weeknum),
         yearweek = as.ordered(yearweek),
         month = month(yearweek, label = TRUE),
         quarter = as.ordered(str_c("Q", quarter(yearweek))),
         year = as.ordered(str_c("Y", year)),
         weeknum = as.ordered(str_c("W", numpad(weeknum))),
         .after = weeknum)

cat_moderators <- mydata %>%
  select(category, risk : purchasefreq) %>%
  distinct()

dv_df <- mydata %>%
  select( id, t,
          # category,
          adawareness : wordofmouth)

iv_df <- mydata %>%
  select( id, t,
          # category,
          adspend : clutter,
          contains(
            c("num_", "hhi_", "ssd_", "nfx_", "cfx_", "tau_", "dfx_", "dau_"))
          )

data_df <- inner_join(dv_df, iv_df)

dm_no_keys <-
  dm(panel_table, time_table,
     data_df,
     cat_moderators)

dm_all_keys <- dm_no_keys %>%
  dm_add_pk(table = data_df, columns = c(id, t)) %>%
  dm_add_pk(table = cat_moderators, columns = category) %>%
  dm_add_pk(table = panel_table, columns = id) %>%
  dm_add_pk(table = time_table, columns = t) %>%

  dm_add_fk(table = data_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
  dm_add_fk(table = panel_table, columns = category, ref_table = cat_moderators, ref_columns = category) %>%
  dm_add_fk(table = data_df, columns = t, ref_table = time_table, ref_columns = t)

dm_all_keys %>%
  dm_examine_constraints()
dm_all_keys %>% dm_draw

dm_all_keys %>% dm_flatten_to_tbl(.start = data_df)

usethis::use_data(datalist_all_keys, overwrite = TRUE)
usethis::use_data(datalist_only_pks, overwrite = TRUE)
