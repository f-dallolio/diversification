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
    adspend = round(adspend / 1000, 2),
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

mydata <- panel_df %>%
  # select(id, t,
  #        adspend,
  #        contains(c("num_", "hhi_", "ssd_"))) %>%
  pivot_longer(cols = contains(c("num_", "hhi_", "ssd_")), names_to = "var") %>%
  mutate(
    name = str_split_i(var, "_", 1),
    var = str_remove_all(var,str_c(name,"_"))
  ) %>%
  pivot_wider() %>%
  mutate(
    neff = 1/hhi,
    neff = case_when(neff > num ~ num, .default = neff),
    hhi = case_when(neff > num ~ 1/neff, .default = hhi),
    nfx = make_nfx(num = num, hhi = hhi),
    cfx = make_cfx(num = num, hhi = hhi),
    tau = make_tau(num = num, hhi = hhi),

    dfx = (1 - cfx) * (num > 1),
    dau = (1 - tau) * (num > 1),
    .after = ssd
  ) %>%
  mutate(
    across(
      contains("cfx","tau","dfx","dau"),
      ~ case_when(neff > num ~ 0, .default = .x)
    )
  ) %>%
  pivot_longer(num : dau)

mydata %>%
  select( id, t, var, name, value ) %>%
  pivot_wider(names_from = "name") %>%
  filter(num > 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  mutate(neff = force_ceiling(1/hhi)) %>%
  filter(neff > num)
%>%
  slice(23) %>% pull(hhi)
  print(n = 100)

%>%
  unite(col = "name",name, var, sep = "_" ) %>%
  pivot_wider()

# mydata <- mydata  %>%
#   pivot_wider(names_from = "name") %>%
#   filter(num > 1) %>%
#   split(.$var) %>%
#   map(~ .x %>% select(-var))

mydata
usethis::use_data(mydata, overwrite = TRUE)

