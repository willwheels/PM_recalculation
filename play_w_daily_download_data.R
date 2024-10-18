library(tidyverse)

TX_2018_download <- readr::read_csv(here::here("ad_viz_plotval_data.csv"))

CA_2019_download <- readr::read_csv(here::here("ad_viz_plotval_data_CA_2019.csv"))

CA_2019_download <- CA_2019_download |>
  janitor::clean_names()

CA_2019_altered <- CA_2019_download |>
  filter(method_code %in% c(236, 238, 736, 738))

## poc is always poc+20 for altered sites, in this data at least.

CA_2019_altered |> select(site_id, poc) |> distinct() 

CA_2019_altered <- CA_2019_altered |>
  select(-poc, - method_description, -daily_aqi_value) |>  # in this data, at least, the poc seems to have changed with method
  pivot_wider(names_from = method_code, values_from = daily_mean_pm2_5_concentration, names_prefix = "pm_") |>
  arrange(site_id, date) |>
  mutate(diff_36 = pm_736 - pm_236, 
         diff_38 = pm_738 - pm_238, 
         diff_pm = if_else(is.na(diff_36), diff_38, diff_36),
         diff_pm_pct = if_else(is.na(diff_36), diff_38/pm_238, diff_36/pm_236 ))

summary(CA_2019_altered)
