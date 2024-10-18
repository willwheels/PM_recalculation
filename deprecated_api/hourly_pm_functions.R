


get_aqs_state_sample_data_api <- function(state_fips, data_year) {
  
  print(state_fips)
  print(data_year)
  
  base_state_sample_url <- "https://aqs.epa.gov/data/api/sampleData/byState"
  
  
  req <-  request(base_url = base_state_sample_url) |>
    req_url_query(`email` = aqs_email, `key` = aqs_key, 
                  `param` = 88101, 
                  `bdate` = paste0(data_year, "0101"),
                  `edate` = paste0(data_year, "1230"),
                  `state` = state_fips,
                  `duration`= "1"
    ) |>
    req_perform()
  
  status_desc <- req |> 
    resp_status_desc()
  
  print(paste("req", status_desc))
  
  response_header_and_data <- req |>
    resp_body_json()
  
  response_sample_header <- response_header_and_data[[1]][[1]] |>
    bind_rows()
  
  response_sample_data <- response_header_and_data[[2]] |>
    bind_rows()
 
  
  Sys.sleep(10)
  
  return(list(response_sample_header, response_sample_data))
  
}

get_aqs_county_sample_data_api <- function(state_fips, county_fips, data_year) {
  
  print(state_fips)
  print(county_fips)
  print(data_year)
  
  base_county_sample_url <- "https://aqs.epa.gov/data/api/sampleData/byCounty"
  
  
  req2 <-   req <- request(base_url = base_county_sample_url) |>
    req_url_query(`email` = aqs_email, `key` = aqs_key, 
                  `param` = 88101, 
                  `bdate` = paste0(data_year, "0101"),
                  `edate` = paste0(data_year, "1231"),
                  `state` = state_fips,
                  `county` = county_fips,
                  `duration`= "1"
    ) |>
    req_perform()
  
  status_desc <- req2 |> 
    resp_status_desc()
  
  print(status_desc)
  
  
  response_header_and_data <- req2 |>
    resp_body_json()
  
  response_sample_header <- response_header_and_data[[1]][[1]] |>
    bind_rows()
  
  response_sample_data <- response_header_and_data[[2]] |>
    bind_rows()
  
  Sys.sleep(10)
  
  
  return(list(response_sample_header, response_sample_data))
  
}


pick_state_or_county_api <- function(state_fips, data_year) {
  
  tidytable::inv_gc()
  
  ## 06 is California, 42 is Pennsylvania--pull by county, otherwise state
  
  if(state_fips %in% c("06", "42")) {
    
    annual_summary_county_scrape <- annual_summary_county |>
      filter(state_code == state_fips, year == data_year)
    
    hourly_sample_data <- purrr::map(annual_summary_county_scrape$county_code,
                                     ~ get_aqs_county_sample_data_api(state_fips = state_fips,
                                                                      county_fips = .x,
                                                                      data_year = data_year))
    hourly_sample_headers <- purrr::map(1:length(hourly_sample_data), 
                                        ~ hourly_sample_data[[.x]][[1]]) |>
      bind_rows()
    
    hourly_sample_obs <- purrr::map(1:length(hourly_sample_data),
                                    ~ hourly_sample_data[[.x]][[2]]) |>
      bind_rows()   
    
    
    
    sample_filename <- paste0("hourly_sample_headers_", state_fips, "_", county_code, "_", data_year , ".Rda")
    
    save(hourly_sample_headers, file = here::here("data", "pm_obs", sample_filename))
    
    data_filename <- paste0("hourly_sample_data_", state_fips, "_", county_code, "_", data_year , ".Rda")
    
    save(hourly_sample_obs, file = here::here("data", "pm_obs", data_filename))
    
    ##
    ## NEED TO ADD BY COUNTY STATUS UPDATE HERE
    ##
    
  } else {
    
    
    hourly_sample_data <- get_aqs_state_sample_data_api(state_fips, data_year)
    
    hourly_sample_headers <- hourly_sample_data[[1]]
    
    hourly_sample_obs <- hourly_sample_data[[2]]
    
    
    sample_filename <- paste0("hourly_sample_headers_", state_fips, "_", data_year , ".Rda")
    
    save(hourly_sample_headers, file = here::here("data", "pm_obs", sample_filename))
    
    data_filename <- paste0("hourly_sample_data_", state_fips, "_", data_year , ".Rda")
    
    save(hourly_sample_obs, file = here::here("data", "pm_obs", data_filename))
    
    load(here::here("data", "scrape_status_state.Rda"))
    
    by_state_status$scrape_status[by_state_status$state_code == state_fips & by_state_status$year == data_year] <- "yes"
    
    save(by_state_status, file = here::here("data", "scrape_status_state.Rda"))
    
    rm(by_state_status)
    
    
  }
  

    
  
  unique(hourly_sample_headers$status)  ## check all success
  
  
  tidytable::inv_gc()
  
}
       

### not sure about this error--maybe just throttled

# [1] "17"
# [1] "2023"
# Error in `map2()`:
#   ℹ In index: 91.
# Caused by error in `req_perform()` at private/pm_air_data/hourly_pm_functions.R:12:3:
#   ! HTTP 422 Unprocessable Entity.    

### 9/16, the following monday

# [1] "17"
# [1] "2023"
# Error in `map2()`:
#   ℹ In index: 1.
# Caused by error in `req_perform()` at private/pm_air_data/hourly_pm_functions.R:12:3:
#   ! Failed to perform HTTP request.
# Caused by error in `curl::curl_fetch_memory()`:
#   ! OpenSSL SSL_read: Connection reset by peer, errno 104
# Run `rlang::last_trace()` to see where the error occurred.
                                 
                                 