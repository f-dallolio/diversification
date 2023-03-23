library(tidyverse)
library(tsibble)
library(fdutils)
library(dm)


load("data/mydata_list.rda")

panel_table <- mydata_list$mydata %>%
  select(id, brand : sector) %>%
  mutate(id = as_factor(id)) %>%
  distinct()

time_table <- mydata_list$mydata %>%
  select(t, weeknum, year) %>%
  distinct() %>%
  mutate(yearweek = make_yearweek(year = year, week = weeknum),
         month = month(yearweek, label = TRUE),
         quarter = as.ordered(str_c("Q", quarter(yearweek))),
         year = as.ordered(str_c("Y", year)),
         weeknum = as.ordered(str_c("W", numpad(weeknum))),
         yearweek = as.ordered(yearweek),
         .after = weeknum)

brand_moderators <- mydata_list$mydata %>%
  select(id, size) %>%
  distinct()

cat_moderators <- mydata_list$mydata %>%
  select(category, risk : purchasefreq) %>%
  distinct()

dv_df <- mydata_list$mydata %>%
  select( id, t,
          category,
          adawareness : wordofmouth)

ad_df <- mydata_list$mydata %>%
  select( id, t,
          category,
          adspend : clutter
          )

data_df <- inner_join(dv_df, ad_df)

dm_no_keys <- new_dm(
  tables = list(
    panel_table, time_table,
    dv_df,
    ad_df,
    brand_moderators,
    cat_moderators
  ) %>%
    c(mydata_list$mydata_div)
)

dm_all_keys <- dm_no_keys %>%
  dm_add_pk(table = panel_table, columns = names(panel_table)) %>%
  dm_add_pk(table = time_table, columns = names(time_table)) %>%

  dm_add_pk(table = dv_df, columns = c(id, t)) %>%
  dm_add_fk(table = dv_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
  dm_add_fk(table = dv_df, columns = t, ref_table = time_table, ref_columns = t) %>%

  dm_add_pk(table = ad_df, columns = c(id, t)) %>%
  dm_add_fk(table = ad_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
  dm_add_fk(table = ad_df, columns = t, ref_table = time_table, ref_columns = t) %>%

  dm_add_pk(table = brand_moderators, columns = id) %>%
  dm_add_fk(table = panel_table, columns = id, ref_table = brand_moderators, ref_columns = id) %>%
  # dm_add_fk(table = dv_df, columns = id, ref_table = brand_moderators, ref_columns = id) %>%
  # dm_add_fk(table = ad_df, columns = id, ref_table = brand_moderators, ref_columns = id) %>%

  dm_add_pk(table = cat_moderators, columns = category) %>%
  dm_add_fk(table = panel_table, columns = category, ref_table = cat_moderators, ref_columns = category)

i=1
for(i in seq_along(names(mydata_list$mydata_div))){

  nm <- names(mydata_list$mydata_div)[i]

  dm_all_keys <-  dm_all_keys %>%
    dm_add_pk(table = {{nm}}, columns = c(id, t)) %>%
    # dm_add_fk(table = dv_df, columns = c(id, t), ref_table = {{nm}}, ref_columns = c(id, t)) %>%
    # dm_add_fk(table = ad_df, columns = c(id, t), ref_table = {{nm}}, ref_columns = c(id, t)) %>%
    dm_add_fk(table = {{nm}}, columns = id, ref_table = panel_table, ref_columns = id) %>%
    dm_add_fk(table = {{nm}}, columns = t, ref_table = time_table, ref_columns = t) #%>%
    # dm_add_fk(table = {{nm}}, columns = id, ref_table = brand_moderators, ref_columns = id)
}

dm_all_keys %>%
  dm_examine_constraints()
dm_all_keys %>% dm_draw

dm_data <- dm_all_keys

usethis::use_data(dm_data, overwrite = TRUE)
