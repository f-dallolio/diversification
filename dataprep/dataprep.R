# load the necessary libraries
library(tidyverse)
library(tsibble)
library(skimr)
library(fdutils)
# remotes::install_github("f-dallolio/diversification")
library(diversification)
library(lubridate)

# data set directory
data_raw_file <- "https://www.dropbox.com/s/fy4kxpeb9v55gf3/diversification_raw.csv?dl=1"

# import data
raw_df <- read_csv(data_raw_file, show_col_types = FALSE) %>% distinct()

# Rename and Transform data
#
panel_df <- raw_df %>%
  transmute(

    # --- identifiers
    id = str_c("id_", numpad(id_panel)),
    t = id_time,

    # --- panel info
    brand = as_factor(brand_name),
    parent = as_factor(parent_company),
    category = as_factor(category),
    sector = as_factor(sector),

    # --- time info
    year = year,
    weeknum = week_num_in_year,

    # ---- dependent variables
    adawareness = adaware,
    brandawareness = aided,
    attention = attention,

    buzz_0 = buzz,
    buzz = minmax(buzz, min = -100, max = 100, to_100 = TRUE),

    consideration = consider,
    currentowner = current_own,
    formerowner = former_own,

    impression_0 = impression,
    impression = minmax(impression, min = -100, max = 100, to_100 = TRUE),

    yougovindex_0 = index,
    yougovindex = minmax(index, min = -100, max = 100, to_100 = TRUE),

    intention = likelybuy,

    perquality_0= quality,
    perquality = minmax(quality, min = -100, max = 100, to_100 = TRUE),

    recommendation_0 = recommend,
    recommendation = minmax(recommend, min = -100, max = 100, to_100 = TRUE),

    reputation_0 = reputation,
    reputation = minmax(reputation, min = -100, max = 100, to_100 = TRUE),

    satisfaction_0 = satisfaction,
    satisfaction = minmax(satisfaction, min = -100, max = 100, to_100 = TRUE),

    pervalue_0 = value,
    pervalue = minmax(value, min = -100, max = 100, to_100 = TRUE),

    wordofmnouth = wom,

    # --- advertising variables
    adspend = total_wk_tv_ads,
    ## -- adspend in $1,000,000
    adspend = round(adspend / 1000, 2),
    ## -- advertising ON (1) or OFF (0)
    adspend_on = as.numeric(adspend > 0),

    # --- diversification variables
    networks_num = num_unique_networks_wk,
    genres_num = num_unique_genres_wk,
    dayparts_num = num_unique_dayparts1_wk,
    dayparts_num_2 = num_unique_dayparts2_wk,
    dayhours_num = num_unique_hours_wk,
    weekdays_num = num_unique_weekdays_wk,
    networks_hhi = hhi_network,
    genres_hhi = hhi_genre,
    dayparts_hhi = hhi_daypart1,
    dayparts_hhi_2 = hhi_daypart2,
    dayhours_hhi = hhi_hours_of_day,
    weekdays_hhi = hhi_days_in_week,
    networks_ssd = stdev_network_share,
    genres_ssd = stdev_genre_share,
    dayparts_ssd = stdev_daypart1_share,
    dayparts_ssd_2 = stdev_daypart2_share,
    dayhours_ssd = stdev_hours_of_day_share,
    weekdays_ssd = stdev_days_in_week_share,
    # --- moderators

    ## -- category - level
    risk = Risk1,
    risk_2 = Risk2,
    risk_3 = Risk3,
    involvement = Involvement1,
    involvement_2 = Involvement2,
    utilitarian = Util_value1,
    utilitarian_2 = Util_value2,
    hedonic = Hedonic_value,
    budgetshare = Share_of_budget,
    purchasefreq = Purchase_frequency
  ) %>%
  group_by(id) %>%
  mutate(
    # --- moderators
    ## -- brand - level
    size = mean(currentowner),
  ) %>%
  # --- create category ad spending and clutter
  group_by(t, category) %>%
  mutate(
    ## -- category total weekly ad spending
    catspend = sum(adspend),
    ## -- weekly number of brands with advertising ON
    catspend_on = sum(catspend > 0),
    .after = adspend_on
  ) %>%
  ungroup() %>%
  ## -- catetgory - level advertising clutter
  mutate(
    clutter = case_when(
      catspend == adspend ~ 0,
      .default = ((catspend - adspend) / (catspend_on - adspend_on))
    ),
    .after = catspend_on
  )

# panel_df <- as_tibble(panel_df, key = "id", index = "t")

div_lup <- div_lookup(x = panel_df, nms = c("_num", "_hhi", "_ssd"))

div_list <- div_lup %>%
  filter(id == 1) %>%
  split(.$variable) %>%
  map(
    ~ .x %>%
      select(- variable, - id) %>%
      as.list() %>%
      unlist()
  ) %>%
  map(
    ~ panel_df %>%
      select(
        c(id, t, category,
          any_of(.x))
      ) %>%
      mutate(
        hhi = hhi * (num > 0)
      )
  ) %>%
  map(
    ~ .x %>%
      mutate(
        nfx = make_nfx(num = num, hhi = hhi),
        cfx = make_cfx(num = num, hhi = hhi),
        tau = make_tau(num = num, hhi = hhi),
        hhi = hhi  * (num > 1),
        dfx = (1 - cfx) * (num > 1),
        dau = (1 - tau) * (num > 1)
      )
  )

dayhours_df <- div_list$dayhours
dayparts_df <- div_list$dayparts
genres_df <- div_list$genres
networks_df <- div_list$networks
weekdays_df <- div_list$weekdays

panel_table <- panel_df %>%
  select( id, brand : sector ) %>%
  distinct()

time_table <- panel_df %>%
  select( t, year, weeknum ) %>%
  distinct() %>%
  mutate(
    yearweek = make_yearweek(
      year = year,
      week = weeknum
    ), .after = t
  ) %>%
  mutate(
    year = str_c("Y", year) %>%
      as.ordered(),
    month = month(yearweek, label = TRUE, abbr = TRUE),
    quarter = str_c("Q", numpad(quarter(yearweek))) %>%
      as.ordered(),
    .after = year
  ) %>%
  mutate(
    across(
      c(t, weeknum),
      ~ as.integer(.x)
    )
  )

dv_df <- panel_df %>%
  select( id, t, category, adawareness : wordofmnouth )

ad_df <- panel_df %>%
  select( id, t, category, adspend : clutter )

brandmod_df <- panel_df %>%
  select( id, size ) %>%
  distinct()

catmod_df <- panel_df %>%
  select( category, risk : purchasefreq ) %>%
  distinct()


datalist <- list(
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

usethis::use_data(datalist, overwrite = TRUE)
#
# library(tidyverse)
# library(dm)
#
# load("data/datalist.rda")
# panel_table <- datalist$panel_table
# time_table <- datalist$time_table
# dv_df <- datalist$dv_df
# ad_df <- datalist$ad_df
# dayhours_df <- datalist$dayhours_df
# dayparts_df <- datalist$dayparts_df
# genres_df <- datalist$genres_df
# networks_df <- datalist$networks_df
# weekdays_df <- datalist$weekdays_df
# brandmod_df <- datalist$brandmod_df
# catmod_df <- datalist$catmod_df
#
# datalist_no_key <- dm(
#   panel_table = panel_table,
#   time_table = time_table,
#   dv_df = dv_df,
#   ad_df = ad_df,
#   dayhours_df = dayhours_df,
#   dayparts_df = dayparts_df,
#   genres_df = genres_df,
#   networks_df = networks_df,
#   weekdays_df = weekdays_df,
#   brandmod_df = brandmod_df,
#   catmod_df = catmod_df
# )
#
# datalist_only_pks <-
#
#   datalist_no_key %>%
#
#   dm_add_pk(table = panel_table, columns = id) %>%
#
#   dm_add_pk(table = time_table, columns = t) %>%
#
#   dm_add_pk(table = dv_df, columns = c(id, t, category)) %>%
#
#   dm_add_pk(table = ad_df, columns = c(id, t, category)) %>%
#
#   dm_add_pk(table = dayhours_df, columns = c(id, t, category)) %>%
#
#   dm_add_pk(table = dayparts_df, columns = c(id, t, category)) %>%
#
#   dm_add_pk(table = genres_df, columns = c(id, t, category)) %>%
#
#   dm_add_pk(table = networks_df, columns = c(id, t, category)) %>%
#
#   dm_add_pk(table = weekdays_df, columns = c(id, t, category)) %>%
#
#   dm_add_pk(table = brandmod_df, columns = id) %>%
#
#   dm_add_pk(table = catmod_df, columns = category)
#
#
# datalist_all_keys <-
#
#   datalist_only_pks %>%
#
#   dm_add_fk(table = panel_table, columns = category, ref_table = catmod_df, ref_columns = category) %>%
#
#   dm_add_fk(table = dv_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
#   dm_add_fk(table = dv_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
#   dm_add_fk(table = dv_df, columns = t, ref_table = time_table, ref_columns = t) %>%
#   dm_add_fk(table = dv_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%
#
#   dm_add_fk(table = ad_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
#   dm_add_fk(table = ad_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
#   dm_add_fk(table = ad_df, columns = t, ref_table = time_table, ref_columns = t) %>%
#   dm_add_fk(table = ad_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%
#
#   dm_add_fk(table = dayhours_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
#   dm_add_fk(table = dayhours_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
#   dm_add_fk(table = dayhours_df, columns = t, ref_table = time_table, ref_columns = t) %>%
#   dm_add_fk(table = dayhours_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%
#
#   dm_add_fk(table = dayparts_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
#   dm_add_fk(table = dayparts_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
#   dm_add_fk(table = dayparts_df, columns = t, ref_table = time_table, ref_columns = t) %>%
#   dm_add_fk(table = dayparts_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%
#
#   dm_add_fk(table = genres_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
#   dm_add_fk(table = genres_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
#   dm_add_fk(table = genres_df, columns = t, ref_table = time_table, ref_columns = t) %>%
#   dm_add_fk(table = genres_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%
#
#   dm_add_fk(table = networks_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
#   dm_add_fk(table = networks_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
#   dm_add_fk(table = networks_df, columns = t, ref_table = time_table, ref_columns = t) %>%
#   dm_add_fk(table = networks_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%
#
#   dm_add_fk(table = weekdays_df, columns = id, ref_table = panel_table, ref_columns = id) %>%
#   dm_add_fk(table = weekdays_df, columns = id, ref_table = brandmod_df, ref_columns = id) %>%
#   dm_add_fk(table = weekdays_df, columns = t, ref_table = time_table, ref_columns = t) %>%
#   dm_add_fk(table = weekdays_df, columns = category, ref_table = catmod_df, ref_columns = category) %>%
#
#   dm_add_fk(table = brandmod_df, columns = id, ref_table = panel_table, ref_columns = id)
#
#
# datalist_all_keys %>%
#   dm_draw(rankdir = "TB", view_type = "all")
#
# datalist_only_pks %>%
#   dm_examine_constraints()
#
# datalist_all_keys %>%
#   dm_examine_constraints()
#
# datalist_all_keys %>%
#   dm_get_all_fks()
#
# usethis::use_data(datalist_all_keys, overwrite = TRUE)
# usethis::use_data(datalist_only_pks, overwrite = TRUE)
#
#
#
#
#
#
#
#
#
#
#
# attr(panel_df, "div_lookup") <- div
#
#
#
#
#
# attr(panel_df, "t0") <-
#
#   panel_df %>%
#   as_tibble() %>%
#   select(t, year, weeknum) %>%
#   distinct() %>%
#   filter(t == 1) %>%
#   mutate(yearweek = make_yearweek(year = year, week = weeknum) - 1) %>%
#   transmute(type = "tsibble::yearweek", t0 = as.character(yearweek)) %>%
#   as.list()
#
# # Panel data and lookup table
#
# id_table <- panel_df %>% select(id, brand:sector)
#
# id_labels <- tibble(
#   id              = "Panelist ID",
#   brand           = "Brand Name",
#   parent          = "Parent Company",
#   category        = "Product Category",
#   sector          = "Sector"
# ) %>% pivot_longer(
#   cols = everything(), names_to = "variable", values_to = "label"
# )
#
# names_id_table <- names(id_table)
#
# # Time data and lookup table
#
# time_table <- panel_df %>% select(t, year, weeknum)
#
# time_labels <- tibble(
#   t               = "Week",
#   year            = "Year",
#   weeknum         = "Week in Year"
# ) %>% pivot_longer(
#   cols = everything(), names_to = "variable", values_to = "label"
# )
#
# names_time_table <- names(time_table)
#
# # Dependent variables data and lookup table
#
# y_data <- panel_df %>% select(id, t, adawareness:wordofmnouth)
#
# y_labels <- tibble(
#   adawareness     = "Advertising Awareness",
#   brandawareness  = "Brand Awareness",
#   attention       = "Attention",
#   buzz            = "Buzz",
#   consideration   = "Purchase Consideration",
#   currentowner    = "Current Owner/User",
#   formerowner     = "Former Owner/User",
#   impression      = "General Impression",
#   yougovindex     = "YouGov Brand Index",
#   intention       = "Purchase Intention",
#   perquality      = "Perceived Quality",
#   recommendation  = "Would Reccomend",
#   reputation      = "Brand Reputation",
#   satisfaction    = "Brand Satisfaction",
#   pervalue.       = "Perceived Value",
#   wordofmnouth    = "Word of Mouth"
# ) %>% pivot_longer(
#   cols = everything(), names_to = "variable", values_to = "label"
# )
#
# names_y_data <- names(y_data)
#
#
# # Advertising variables data and lookup table
#
# ad_data <- panel_df %>% select(id, t, adspend:clutter)
#
# ad_labels <- tibble(
#   adspend         = "Adverttising sSpending",
#   adspend_on      = "Advertising Spending ON/OFF (1/0)",
#   catspend        = "Category Weekly Adverttising Spending",
#   catspend_on     = "Brands with Ad Spend ON in the Category",
#   clutter         = "Advertising Clutter in the Category",
# ) %>% pivot_longer(
#   cols = everything(), names_to = "variable", values_to = "label"
# )
#
# names_ad_data <- names(ad_data)
#
# # Diversification/Concentration variables data and lookup table
#
# div_data <- panel_df %>% select(id, t, contains( c("_num", "_hhi", "_ssd") ) )
#
# div_labels_list <- list(
#
#   div_var = tibble(
#     num           = "number of different outputs",
#     hhi           = "Concentration",
#     cfx           = "Relative Concentration",
#     nfx           = "Relative Number of Outputs",
#     tau           = "Relative Concentration Index",
#     dfx           = "Diversification",
#     dau           = "Relative Diversification"
#   ) %>% pivot_longer(
#     cols = everything(), names_to = "variable", values_to = "label"
#   ),
#
#   div_id = tibble(
#     networks_num    = "Networks",
#     genres_num      = "Genres",
#     dayparts1_num   = "Dayparts",
#     dayparts2_num   = "Dayparts",
#     dayhours_num    = "Hours of Day",
#     weekdays_num    = "Days of the Week",
#   ) %>% pivot_longer(
#     cols = everything(), names_to = "variable", values_to = "label"
#   )
#
# )
#
# # Moderators data and lookup table
#
# moder_data <- panel_df %>% select(id, t, category, risk:size)
#
# moder_labels <- tibble(
#   risk            = "Category Perceived Risk",
#   risk2           = "Category Perceived Risk",
#   risk3           = "Category Perceived Risk",
#   involvement     = "Categorty Involvement",
#   involvement_2   = "Categorty Involvement",
#   utilitarian     = "Categorty Utilitarian Value",
#   utilitarian_2   = "Categorty Utilitarian Value",
#   hedonic         = "Categorty Hedonic Value",
#   budgetshare     = "Category Share of Household Budget",
#   purchasefreq    = "Category Purchase Frequency",
#   size            = "Brand Size"
# ) %>% pivot_longer(
#   cols = everything(), names_to = "variable", values_to = "label"
# )
#
# names_moder_data <- names(moder_data)
#
# ### List of data
#
# data_list <- list(
#   id_table,
#   time_table,
#   y_data,
#   ad_data,
#   div_data,
#   moder_data,
#   panel_df
# )
#
# ### List of labels
#
#
# data_labels <- list(
#   id_labels,
#   time_labels,
#   y_labels,
#   ad_labels,
#   div_labels_list,
#   moder_labels,
# )
#
# ## Save **`data_list`** and **`data_labels`** as `.rds` file
#
# saveRDS(data_list, "data-raw/datalist.rds")
# saveRDS(data_labels, "data-raw/datalabels.rds")
#
#
# ###
# datalist <- readRDS("data-raw/datalist.rds")
# datalabels <- readRDS("data-raw/datalabels.rds")
#
# usethis::use_data(datalist, overwrite = TRUE)
# usethis::use_data(datalabels, overwrite = TRUE)
