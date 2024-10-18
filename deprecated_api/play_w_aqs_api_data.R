library(RAQSAPI)


aqs_email <- getPass::getPass(msg = "AQS email: ", noblank = TRUE, forcemask = FALSE)

aqs_key <- getPass::getPass(msg = "AQS email: ", noblank = TRUE, forcemask = FALSE)


aqs_credentials(username = aqs_email, key = aqs_key)

test <- aqs_sampledata_by_state(parameter = "88101", 
                                bdate = lubridate::mdy("01-01-2018"),
                                edate = lubridate::mdy("12-31-2018"),
                                stateFIPS = "37",
                                )

## poc is always poc+20 for altered sites, in this data at least.
test_altered <- test |> 
  filter(method_code %in% c(236, 238, 736, 738))

test_altered |> select(site_number, poc) |> distinct() 

## this isn't right yet


test_altered <- test_altered |>
  select(-method, -poc) |>
  pivot_wider(names_from = method_code, values_from = sample_measurement, names_prefix = "pm_") |>
  arrange(site_number, date_local, time_local) |>
  mutate(diff_36 = pm_736 - pm_236,
         diff_38 = pm_738 - pm_238,
         diff_pm = if_else(is.na(diff_36), diff_38, diff_36),
         diff_pm_pct = if_else(is.na(diff_36), diff_38/pm_238, diff_36/pm_236 ))

summary(test_altered)


## according to supplemental info, method should be switched to SPM but that does not appear to have occurred
unique(test$method_type)


check_state  <- aqs_annualsummary_by_state(parameter = "88101",
                            bdate = as.Date("20170101",
                                            format="%Y%m%d"),
                            edate = as.Date("20231231",
                                            format="%Y%m%d"),
                            stateFIPS = "37"
                      
  )

check_state_filtered <- check_state |>
  filter(metric_used == "Observed Values") |>
  arrange(site_number)
## 1.8 million for state over all years