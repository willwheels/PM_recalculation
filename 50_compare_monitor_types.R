library(tidytable)


pm_hourly_files <- list.files(here::here("data", "pm_downloads", "unzipped_files"),
                              full.names = TRUE)

pm_hourly_files <- stringr::str_subset(pm_hourly_files, ".csv$")

## should probably drop max! but any other stats or comparisons?

summarise_pm_data <- function(pm_df) {
  
  pm_df_summ <- pm_df |>
    group_by(sensor_type) |>
    summarise(num_readings = n(),
              mean = mean(sample_measurement),
              median = median(sample_measurement),
              maximum = max(sample_measurement)) |>
    ungroup() 
  
}


process_hourly <- function(filename){

  print(filename)
  pm_data <- fread(filename)
  
  pm_data <- pm_data |>
    janitor::clean_names() |>
    mutate(sensor_type = case_when(method_code %in% c(236, 238) ~ "uncorrected",
                                   method_code %in% c(736, 738) ~ "corrected",
                                   !method_code %in% c(236, 238, 736, 738) ~ "unaffected"),
           sensor_type = factor(sensor_type, levels = c("unaffected", "uncorrected", "corrected")),
           measurement_date = lubridate::ymd(date_local)
           )
  
  #pm_data |> group_by(sensor_type) |> count()
  
  data_year <- stringr::str_extract(filename, "\\d{4}.csv")
  data_year <- stringr::str_extract(data_year, "\\d{4}")
  
 
   
  
  pm_data_hourly_summ <- summarise_pm_data(pm_data) 
  
  pm_data_hourly_summ <- pm_data_hourly_summ |> 
    mutate(data_year = data_year)
  
  pm_data_daily <- pm_data |>
    group_by(state_code, county_code, site_num, measurement_date, sensor_type) |>
    summarise(sample_measurement = mean(sample_measurement)) |>
    ungroup()
  
  pm_data_daily_summ <- summarise_pm_data(pm_data_daily) 
  
  pm_data_daily_summ <- pm_data_daily_summ |>
    mutate(data_year = data_year)
  
  pm_data_monthly <- pm_data |>
    mutate(measurement_month = lubridate::month(measurement_date)) |>
    group_by(state_code, county_code, site_num, measurement_month, sensor_type) |>
    summarise(sample_measurement = mean(sample_measurement)) |>
    ungroup()
  
  pm_data_monthly_summ <- summarise_pm_data(pm_data_monthly)
    
  pm_data_monthly_summ <- pm_data_monthly_summ |> 
    mutate(data_year = data_year)
  
  return(list(pm_data_hourly_summ, pm_data_daily_summ, pm_data_monthly_summ))

}


all_hourly_processed <- purrr::map(pm_hourly_files, process_hourly)

all_hourly_summed <- purrr::map(all_hourly_processed, ~ .x[1]) |>
  bind_rows()


all_daily_summed <- purrr::map(all_hourly_processed, ~ .x[2]) |>
  bind_rows()


all_monthly_summed <- purrr::map(all_hourly_processed, ~ .x[2]) |>
  bind_rows()
