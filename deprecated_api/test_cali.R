
library(RAQSAPI)
library(dplyr)

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


aqs_return_CA_pm_1 <- aqs_sampledata_by_state(parameter = "88101", 
                                      bdate = lubridate::mdy(paste0("01-01-2017")),
                                      edate = lubridate::mdy(paste0("12-31-2017")),
                                      stateFIPS = "06",
                                      duration = "1",
                                      return_header = FALSE)


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

nrow(aqs_return_CA_pm_2) + nrow(aqs_return_CA_pm_3)

