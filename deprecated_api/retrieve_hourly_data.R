
library(RAQSAPI)
library(dplyr)


aqs_email <- getPass::getPass(msg = "AQS email: ", noblank = TRUE, forcemask = FALSE)


aqs_key <- getPass::getPass(msg = "AQS key: ", noblank = TRUE, forcemask = FALSE)


aqs_credentials(username = aqs_email, key = aqs_key)


load(here::here("data", "annual_summary_data.Rda"))

annual_counts <- all_annual_summaries |>
  group_by(state, state_code, year) |>
  summarise(total_obs = sum(observation_count)) |>
  ungroup() |>
  arrange(desc(total_obs))
     
## I swear when I first started doing this, I was getting counts over 1,000,000
## requests for a state-year with more than 1,000,000 obs will get rejected

annual_counts |>
  filter(total_obs>1000000)
     

get_one_state_pm_obs <- function(state_fips, data_year) {
  
  print(state_fips)
  print(data_year)
  
  aqs_return <- aqs_sampledata_by_state(parameter = "88101", 
                          bdate = lubridate::mdy(paste0("01-01-", data_year)),
                          edate = lubridate::mdy(paste0("06-30-", data_year)),
                          stateFIPS = state_fips,
                          duration = "1",
                          return_header = TRUE)
  
  header <- aqs_return[[1]]$Header
  
  data <- aqs_removeheader(aqs_return)
  
  
  header_filename <- paste0(state_fips, "_", data_year, "_pm_header.Rda")
  data_filename <- paste0(state_fips,  "_", data_year, "_pm_data.Rda")
  
  
  save(data, file = here::here("data", "pm_obs", data_filename))
  save(header, file = here::here("data", "pm_obs", header_filename))
  
  walk_df_fn <- readRDS(here::here("data", "scraping_tracking.RDS"))
  walk_df_fn$read[walk_df_fn$state_code == state_fips & walk_df_fn$all_years == data_year] <- "yes"
  saveRDS(walk_df_fn, here::here("data", "scraping_tracking.RDS"))
    
  ## server denied request after fips 5 with ten second delay in function
  print("resting")
  Sys.sleep(15)
  

  
}
  

## below only needs to be run once

# all_years <- 2017:2023
# all_years <- as.character(all_years)
# 
# all_fips <- tigris::fips_codes |>
#   select(state, state_code) |>
#   distinct() |>
#   filter(state %in% c(state.abb, "DC"))
# 
# all_years
# 
# walk_df <- tidyr::expand_grid(all_fips, all_years)
# 
# walk_df$read <- "no"
# 
# saveRDS(walk_df, file = here::here("data", "scraping_tracking.RDS"))

## above only needs to be run once

walk_df_purr <- readRDS(here::here("data", "scraping_tracking.RDS"))

walk_df_purr <- walk_df_purr |>
  filter(read == "no")

all_returns <- purrr::walk2(walk_df_purr$state_code, walk_df_purr$all_years,
                          get_one_state_pm_obs)

## got through Arkansas, FIPS 5


