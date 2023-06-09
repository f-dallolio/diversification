# load the necessary libraries
library(tidyverse)
library(tsibble)
library(skimr)
remotes::install_github("f-dallolio/fdutils")
library(fdutils)
remotes::install_github("f-dallolio/diversification")
library(diversification)
library(lubridate)
library(ggplot2)

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

    buzz0 = buzz,
    buzz = minmax(buzz, min = -100, max = 100, to_100 = TRUE),

    consideration = consider,
    currentowner = current_own,
    formerowner = former_own,

    impression0 = impression,
    impression = minmax(impression, min = -100, max = 100, to_100 = TRUE),

    yougovindex0 = index,
    yougovindex = minmax(index, min = -100, max = 100, to_100 = TRUE),

    intention = likelybuy,

    perquality0= quality,
    perquality = minmax(quality, min = -100, max = 100, to_100 = TRUE),

    recommendation0 = recommend,
    recommendation = minmax(recommend, min = -100, max = 100, to_100 = TRUE),

    reputation0 = reputation,
    reputation = minmax(reputation, min = -100, max = 100, to_100 = TRUE),

    satisfaction0 = satisfaction,
    satisfaction = minmax(satisfaction, min = -100, max = 100, to_100 = TRUE),

    pervalue0 = value,
    pervalue = minmax(value, min = -100, max = 100, to_100 = TRUE),

    wordofmouth = wom,

    # --- advertising variables
    adspend = total_wk_tv_ads,
    ## -- adspend in $1,000,000
    adspend = round(adspend / 1000),
    ## -- advertising ON (1) or OFF (0)
    adspend01 = as.numeric(adspend > 0),

    # --- diversification variables
    num_networks = num_unique_networks_wk,
    num_genres = num_unique_genres_wk,
    num_dayparts = num_unique_dayparts1_wk,
    num_dayparts2 = num_unique_dayparts2_wk,
    num_dayhours = num_unique_hours_wk,
    num_weekdays = num_unique_weekdays_wk,
    hhi_networks = hhi_network,
    hhi_genres = hhi_genre,
    hhi_dayparts = hhi_daypart1,
    hhi_dayparts2 = hhi_daypart2,
    hhi_dayhours = hhi_hours_of_day,
    hhi_weekdays = hhi_days_in_week,
    ssd_networks = stdev_network_share,
    ssd_genres = stdev_genre_share,
    ssd_dayparts = stdev_daypart1_share,
    ssd_dayparts2 = stdev_daypart2_share,
    ssd_dayhours = stdev_hours_of_day_share,
    ssd_weekdays = stdev_days_in_week_share,
    # --- moderators

    ## -- category - level
    risk = Risk1,
    risk2 = Risk2,
    risk3 = Risk3,
    involvement = Involvement1,
    involvement2 = Involvement2,
    utilitarian = Util_value1,
    utilitarian2 = Util_value2,
    hedonic = Hedonic_value,
    budgetshare = Share_of_budget,
    purchasefreq = Purchase_frequency
  ) %>%
  mutate(across(contains(c("num_", "hhi_", "ssd_")), ~ .x * adspend01)) %>%
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
    catspend01 = sum(catspend > 0),
    .after = adspend01
  ) %>%
  ungroup() %>%
  ## -- catetgory - level advertising clutter
  mutate(
    clutter = case_when(
      catspend == adspend ~ 0,
      .default = ((catspend - adspend) / (catspend01 - adspend01))
    ),
    .after = catspend01
  )

mydata_df <-
  panel_df %>%
  select(
    !contains(c("num_", "hhi_", "ssd_"))
  )

mydata_div_temp <- panel_df %>%
  # filter(adspend > 0) %>%
  select(id, t,
         adspend,
         contains(c("num_", "hhi_", "ssd_"))) %>%
  pivot_longer(cols = contains(c("num_", "hhi_", "ssd_")), names_to = "variable") %>%
  mutate(
    name = str_split_i(variable, "_", 1),
    variable = str_remove_all(variable,str_c(name,"_"))
  ) %>%
  pivot_wider()

mydata_div_n <- mydata_div_temp %>%
  filter(num > 1) %>%
  mutate(
    nef = 1/hhi,
    ssd = if_else(nef >= num, 0, ssd),
    cfx = (num * hhi - 1) / (num - 1),
    cfx = if_else(nef >= num, 0, cfx),
    tau = sqrt(cfx),
    nfx = hhi - cfx,
    nef = if_else(nef >= num, num, nef),
  ) %>%
  relocate(nfx, .after = nef)

mydata_div_1 <- mydata_div_temp %>%
  filter(num == 1) %>%
  mutate(
    nef = 1,
    hhi = 1,
    ssd = 0,
    cfx = 1,
    tau = 1,
    nfx = 0,
  ) %>%
  relocate(nfx, .after = nef)

mydata_div <- mydata_div_temp %>%
  select(id, t, adspend, variable) %>%
  left_join(mydata_div_n) %>%
  left_join(mydata_div_1) %>%
  # filter(adspend>0) %>%
  mutate(across(c(num:tau), ~ if_else(is.na(.x), 0, .x) )) %>%
  split(.$variable)


rm(
  data_raw_file,
  raw_df,
  panel_df,
  mydata_div_temp,
  mydata_div_n,
  mydata_div_1
)


panel_table <- mydata_df %>%
  select(id, brand : sector) %>%
  mutate(id = as_factor(id)) %>%
  distinct()

time_table <- mydata_df %>%
  select(t, weeknum, year) %>%
  distinct() %>%
  mutate(yearweek = make_yearweek(year = year, week = weeknum),
         month = month(yearweek, label = TRUE),
         quarter = as.ordered(str_c("Q", quarter(yearweek))),
         year = as.ordered(str_c("Y", year)),
         weeknum = as.ordered(str_c("W", numpad(weeknum))),
         yearweek = as.ordered(yearweek),
         .after = weeknum)

brand_moderators <- mydata_df %>%
  select(id, size) %>%
  distinct()

cat_moderators <- mydata_df %>%
  select(category, risk : purchasefreq) %>%
  distinct()

dv_df <- mydata_df %>%
  select( id, t,
          category,
          adawareness : wordofmouth)

ad_df <- mydata_df %>%
  select( id, t,
          category,
          adspend : clutter
  )

data_df <- inner_join(dv_df, ad_df)


usethis::use_data(mydata_list, overwrite = TRUE)
