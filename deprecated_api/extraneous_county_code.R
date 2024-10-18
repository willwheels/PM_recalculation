
annual_summary_scrape <- all_annual_summaries |>
  select(state_code, county_code, year) |>
  mutate(year = as.character(year)) |>
  distinct()


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
  
  Sys.sleep(5)
  
  
  return(list(response_sample_header, response_sample_data))
  
}



