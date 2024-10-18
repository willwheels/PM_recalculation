``` r
library(RAQSAPI)
#> Use the function
#> RAQSAPI::aqs_credentials(username, key)
#> before using other RAQSAPI functions
#> See ?RAQSAPI::aqs_credentials for more information
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

aqs_email <- "wheeler.william@epa.gov"

aqs_key <- "carmelcat43"

aqs_credentials(username = aqs_email, key = aqs_key)



aqs_return_CA_pm_summary <- aqs_annualsummary_by_state(parameter = "88101", 
                                                        bdate = lubridate::mdy(paste0("01-01-2017")),
                                                        edate = lubridate::mdy(paste0("12-31-2017")),
                                                        stateFIPS = "06",
                                                        return_header = FALSE)

aqs_return_CA_pm_summary |>
  filter(sample_duration_code == "1") |>
  summarise(total_obs = sum(observation_count))
#>   total_obs
#> 1    868623


aqs_return_CA_pm_1 <- aqs_sampledata_by_state(parameter = "88101", 
                                      bdate = lubridate::mdy(paste0("01-01-2017")),
                                      edate = lubridate::mdy(paste0("12-31-2017")),
                                      stateFIPS = "06",
                                      duration = "1",
                                      return_header = FALSE)
#> Waiting 4s for throttling delay ■■■■■■■■
#> Waiting 4s for throttling delay ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Waiting 4s for throttling delay ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#> Error in `purrr::pmap()`:
#> ℹ In index: 1.
#> Caused by error in `req_perform()`:
#> ! Failed to perform HTTP request.
#> Caused by error in `curl::curl_fetch_memory()`:
#> ! OpenSSL SSL_read: Connection reset by peer, errno 104


aqs_return_CA_pm_2 <- aqs_sampledata_by_state(parameter = "88101", 
                                              bdate = lubridate::mdy(paste0("01-01-2017")),
                                              edate = lubridate::mdy(paste0("06-30-2017")),
                                              stateFIPS = "06",
                                              duration = "1",
                                              return_header = FALSE)


aqs_return_CA_pm_3 <- aqs_sampledata_by_state(parameter = "88101", 
                                              bdate = lubridate::mdy(paste0("07-01-2017")),
                                              edate = lubridate::mdy(paste0("12-31-2017")),
                                              stateFIPS = "06",
                                              duration = "1",
                                              return_header = FALSE)

aqs_return_CA_pm_summary |>
  filter(sample_duration_code == "1") |>
  summarise(total_obs = sum(observation_count))
#>   total_obs
#> 1    868623

nrow(aqs_return_CA_pm_2) + nrow(aqs_return_CA_pm_3)
#> [1] 657388
```

<sup>Created on 2024-09-11 with [reprex v2.1.0](https://reprex.tidyverse.org)</sup>
