
library(RAQSAPI)
library(dplyr)


aqs_email <- getPass::getPass(msg = "AQS email: ", noblank = TRUE, forcemask = FALSE)

aqs_key <- getPass::getPass(msg = "AQS email: ", noblank = TRUE, forcemask = FALSE)


aqs_credentials(username = aqs_email, key = aqs_key)


start_year <- "2017"
end_year <- "2017"


get_state_counts <- function(state_fips) {
  
  
  aqs_return <- aqs_annualsummary_by_state(parameter = "88101", 
                                           bdate = lubridate::mdy(paste0("01-01-", start_year)),
                                           edate = lubridate::mdy(paste0("12-31-", end_year)),
                                           stateFIPS = state_fips,
                                           return_header = TRUE)
  
  headers <- purrr::map(1:length(aqs_return), ~aqs_return[[.x]]$Header)
  
  headers <- headers |>
    bind_rows()
  
  data <- aqs_removeheader(aqs_return) |>
    #filter(sample_duration == "1 HOUR", metric_used == "Observed Values") |> # should be equivalent to duration_code = 1
    group_by(state_code, county_code, site_number, poc, year) |>
    arrange(desc(observation_count)) |>  ## a monitor can have different counts based on exceptional events, we just want the biggest number
    #slice(1) |>
    ungroup()
  
  return(list(headers, data))
  
}

all_fips <- tigris::fips_codes |>
  select(state, state_code) |>
  distinct() |>
  filter(state %in% c(state.abb, "DC"))

all_annual_summaries <- purrr::map(all_fips$state_code, 
                          get_state_counts)

all_headers <- purrr::map(1:length(all_annual_summaries), ~ all_annual_summaries[[.x]][[1]]) |>
  bind_rows()

unique(all_headers$status)  ## check all success

save(all_headers, file = here::here("data", "annual_summary_headers.Rda"))

all_annual_summaries <- purrr::map(1:length(all_annual_summaries), ~ all_annual_summaries[[.x]][[2]]) |>
  bind_rows()

save(all_annual_summaries, file = here::here("data", "annual_summary_data.Rda"))



