#| label: load packages

# load the necessary libraries
library(tidyverse)
library(tsibble)
library(skimr)
library(fdutils)
remotes::install_github("f-dallolio/diversification")
library(diversification)

# data set directory
data_raw_file <- "data_prep/diversification_raw.csv"

# import data
raw_df <- read_csv(data_raw_file, show_col_types = FALSE) %>% distinct()


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
    buzz = buzz,
    consideration = consider,
    currentowner = current_own,
    formerowner = former_own,
    impression. = impression,
    ## -- impression from (-100, 100) to (0,100)
    impression = minmax(impression, min = -100, max = 100, to_100 = TRUE),
    yougovindex = index,
    intention = likelybuy,
    perquality = quality,
    recommendation = recommend,
    reputation = reputation,
    satisfaction = satisfaction,
    pervalue. = value,
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

skim(panel_df)

div_lookup(x = panel_df)
