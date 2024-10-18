
library(httr2)
library(dplyr)

source(here::here("hourly_pm_functions.R"))

aqs_email <- getPass::getPass(msg = "AQS email: ", noblank = TRUE, forcemask = FALSE)

aqs_key <- getPass::getPass(msg = "AQS key: ", noblank = TRUE, forcemask = FALSE)


load(here::here("data", "annual_summary_data.Rda"))



## take annual summaries and reduce to only state/year combos with obs
## I tried doing this at the county level, but there are 3310 rows in that case
## and you can only make 

annual_summary_state <- all_annual_summaries |>
  select(state_code, year) |>
  mutate(year = as.character(year)) |>
  distinct()

annual_summary_county <- all_annual_summaries |>
  select(state_code, county_code, year) |>
  mutate(year = as.character(year)) |>
  distinct()

### create status lists
### should only need to run once at beginning of scrape

# by_state_status <- annual_summary_state |>
#   filter(!state_code %in% c("06", "42")) |>
#   mutate(scrape_status = "no")
# 
# save(by_state_status, file = here::here("data", "scrape_status_state.Rda"))



## walk over the states where you go by state/year combo

load(here::here("data", "scrape_status_state.Rda"))

walk_by_state <- by_state_status |>
  filter(scrape_status == "no")

rm(by_state_status)

purrr::walk2(walk_by_state$state_code, walk_by_state$year,
      pick_state_or_county_api)

