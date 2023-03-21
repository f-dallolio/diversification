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

    wordofmouth = wom,

    # --- advertising variables
    adspend = total_wk_tv_ads,
    ## -- adspend in $1,000,000
    adspend = round(adspend / 1000, 2),
    ## -- advertising ON (1) or OFF (0)
    adspend_on = as.numeric(adspend > 0),

    # --- diversification variables
    num_networks = num_unique_networks_wk,
    num_genres = num_unique_genres_wk,
    num_dayparts = num_unique_dayparts1_wk,
    num_dayparts_2 = num_unique_dayparts2_wk,
    num_dayhours = num_unique_hours_wk,
    num_weekdays = num_unique_weekdays_wk,
    hhi_networks = hhi_network,
    hhi_genres = hhi_genre,
    hhi_dayparts = hhi_daypart1,
    hhi_dayparts_2 = hhi_daypart2,
    hhi_dayhours = hhi_hours_of_day,
    hhi_weekdays = hhi_days_in_week,
    ssd_networks = stdev_network_share,
    ssd_genres = stdev_genre_share,
    ssd_dayparts = stdev_daypart1_share,
    ssd_dayparts_2 = stdev_daypart2_share,
    ssd_dayhours = stdev_hours_of_day_share,
    ssd_weekdays = stdev_days_in_week_share,
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

mydata <- panel_df %>%
  pivot_longer(cols = contains(c("num_", "hhi_", "ssd_")), names_to = "var") %>%
  mutate(
    name = str_split_i(var, "_", 1),
    var = str_remove_all(var,str_c(name,"_"))
  ) %>%
  pivot_wider() %>%
  mutate(
    nfx = make_nfx(num = num, hhi = hhi),
    cfx = make_cfx(num = num, hhi = hhi),
    tau = make_tau(num = num, hhi = hhi),
    hhi = hhi  * (num > 1),
    dfx = (1 - cfx) * (num > 1),
    dau = (1 - tau) * (num > 1),
    .after = ssd
  ) %>%
  pivot_longer(num : dau) %>%
  unite(col = "name",name, var, sep = "_" ) %>%
  pivot_wider()

mydata
usethis::use_data(mydata, overwrite = TRUE)

