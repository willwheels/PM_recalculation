library(httr2)
library(dplyr)


aqs_email <- getPass::getPass(msg = "AQS email: ", noblank = TRUE, forcemask = FALSE)

aqs_key <- getPass::getPass(msg = "AQS email: ", noblank = TRUE, forcemask = FALSE)



get_aqs_annual_summary_api <- function(state_fips, data_year) {
  
  print(state_fips)
  print(data_year)
  
  base_annual_summ_url <- "https://aqs.epa.gov/data/api/annualData/byState"
  
  req <- request(base_url = base_annual_summ_url) |>
    req_url_query(`email` = aqs_email, `key` = aqs_key, 
                  `param` = 88101, 
                  `bdate` = paste0(data_year, "0101"),
                  `edate` = paste0(data_year, "1231"),
                  `state` = state_fips,
                  `duration`= "1"
                  ) |>
    req_perform()
  
  status_desc <- req |> 
    resp_status_desc()
  
  print(status_desc)
  
  response_header_and_data <- req |>
    resp_body_json()
  
  response_header <- response_header_and_data[[1]][[1]] |>
    bind_rows()
  
  response_data <- response_header_and_data[[2]] |>
    bind_rows()
  
  Sys.sleep(10)
  
  return(list(response_header, response_data))
  
}

all_fips <- tigris::fips_codes |>
  select(state, state_code) |>
  distinct() |>
  filter(state %in% c(state.abb, "DC"))

all_years <- as.character(2017:2023)

map_df <- tidyr::expand_grid(all_fips, all_years)

all_annual_summaries <- purrr::map2(map_df$state_code, map_df$all_years,
                                    get_aqs_annual_summary_api)


all_headers <- purrr::map(1:length(all_annual_summaries), ~ all_annual_summaries[[.x]][[1]]) |>
  bind_rows()

unique(all_headers$status)  ## check all success

save(all_headers, file = here::here("data", "annual_summary_headers.Rda"))

all_annual_summaries <- purrr::map(1:length(all_annual_summaries), ~ all_annual_summaries[[.x]][[2]]) |>
  bind_rows()

save(all_annual_summaries, file = here::here("data", "annual_summary_data.Rda"))

