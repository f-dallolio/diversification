library(tidyverse)
library(dm)

load("data/datalist.rda")
panel_table <- datalist$panel_table
time_table <- datalist$time_table
dv_df <- datalist$dv_df
ad_df <- datalist$ad_df
dayhours_df <- datalist$dayhours_df
dayparts_df <- datalist$dayparts_df
genres_df <- datalist$genres_df
networks_df <- datalist$networks_df
weekdays_df <- datalist$weekdays_df
brandmod_df <- datalist$brandmod_df
catmod_df <- datalist$catmod_df

datalist_no_key <- dm(
  panel_table = panel_table,
  time_table = time_table,
  dv_df = dv_df,
  ad_df = ad_df,
  dayhours_df = dayhours_df,
  dayparts_df = dayparts_df,
  genres_df = genres_df,
  networks_df = networks_df,
  weekdays_df = weekdays_df,
  brandmod_df = brandmod_df,
  catmod_df = catmod_df
)

datalist_only_pks <-

  datalist_no_key %>%

  dm_add_pk(table = panel_table, columns = id) %>%

  dm_add_pk(table = time_table, columns = t) %>%

  dm_add_pk(table = dv_df, columns = c(id, t, category)) %>%

  dm_add_pk(table = ad_df, columns = c(id, t, category)) %>%

  dm_add_pk(table = dayhours_df, columns = c(id, t, category)) %>%

  dm_add_pk(table = dayparts_df, columns = c(id, t, category)) %>%

  dm_add_pk(table = genres_df, columns = c(id, t, category)) %>%

  dm_add_pk(table = networks_df, columns = c(id, t, category)) %>%

  dm_add_pk(table = weekdays_df, columns = c(id, t, category)) %>%

  dm_add_pk(table = brandmod_df, columns = id) %>%

  dm_add_pk(table = catmod_df, columns = category)


datalist_all_keys <-

  datalist_only_pks %>%

  dm_add_fk(table = panel_table, columns = category, ref_table = catmod_df, ref_columns = category) %>%

  dm_add_fk(table = dv_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
  dm_add_fk(table = dv_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
  dm_add_fk(table = dv_df, columns = t, ref_table = time_table, ref_columns = t) %>%
  dm_add_fk(table = dv_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%

  dm_add_fk(table = ad_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
  dm_add_fk(table = ad_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
  dm_add_fk(table = ad_df, columns = t, ref_table = time_table, ref_columns = t) %>%
  dm_add_fk(table = ad_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%

  dm_add_fk(table = dayhours_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
  dm_add_fk(table = dayhours_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
  dm_add_fk(table = dayhours_df, columns = t, ref_table = time_table, ref_columns = t) %>%
  dm_add_fk(table = dayhours_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%

  dm_add_fk(table = dayparts_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
  dm_add_fk(table = dayparts_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
  dm_add_fk(table = dayparts_df, columns = t, ref_table = time_table, ref_columns = t) %>%
  dm_add_fk(table = dayparts_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%

  dm_add_fk(table = genres_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
  dm_add_fk(table = genres_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
  dm_add_fk(table = genres_df, columns = t, ref_table = time_table, ref_columns = t) %>%
  dm_add_fk(table = genres_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%

  dm_add_fk(table = networks_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
  dm_add_fk(table = networks_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
  dm_add_fk(table = networks_df, columns = t, ref_table = time_table, ref_columns = t) %>%
  dm_add_fk(table = networks_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%

  dm_add_fk(table = weekdays_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
  dm_add_fk(table = weekdays_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
  dm_add_fk(table = weekdays_df, columns = t, ref_table = time_table, ref_columns = t) %>%
  dm_add_fk(table = weekdays_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%

  dm_add_fk(table = brandmod_df, columns = id, ref_table = panel_table, ref_columns = id)


datalist_all_keys %>%
  dm_draw(rankdir = "TB", view_type = "all")

datalist_only_pks %>%
  dm_examine_constraints()

datalist_all_keys %>%
  dm_examine_constraints()

datalist_all_keys %>%
  dm_get_all_fks()

usethis::use_data(datalist_all_keys, overwrite = TRUE)
usethis::use_data(datalist_only_pks, overwrite = TRUE)
