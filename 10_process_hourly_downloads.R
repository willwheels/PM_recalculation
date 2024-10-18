library(tidytable)
#library(ggplot2)

all_unzips <- list.files(here::here("data", "pm_downloads", "unzipped_files"),
                       full.names = TRUE)


process_raw_pm_data <- function(pm_data_file) {
  
  inv_gc()
  
  pm_data <- fread(pm_data_file)
  
  pm_data <- pm_data |>
    janitor::clean_names() |> 
    rowwise() |>
    mutate(identifier = paste(state_code, county_code, site_num, sep = "_")) |>
    ungroup() 
  
  data_year <- stringr::str_extract(pm_data_file, "20\\d\\d")
  
  monitor_data <- pm_data |>
    group_by(identifier, poc, state_code, latitude, longitude, datum, method_type, method_code) |>
    count() |>
    mutate(data_year = data_year) |>
    ungroup()
  
  pm_data <- pm_data |>
    filter(method_code %in% c(236, 238, 736, 738)) |>
    mutate(poc_match = if_else(poc > 20, poc-20, poc)) |>     ## new poc is always old poc+20 for altered sites, in all the data I've checked, should be verified
    arrange(site_num, poc_match, date_local, time_local) |>
    select(-method_name, -poc, -date_of_last_change) |>
    pivot_wider(names_from = method_code, values_from = sample_measurement, names_prefix = "pm_") |>
    arrange(site_num, date_local, time_local)

  
  if(!"pm_236" %in% colnames(pm_data)) { pm_236  = NA_real_}
  if(!"pm_238" %in% colnames(pm_data)) { pm_238  = NA_real_}
  if(!"pm_736" %in% colnames(pm_data)) { pm_736 = NA_real_}
  if(!"pm_738" %in% colnames(pm_data)) { pm_738 = NA_real_}
  
  pm_data <-   pm_data |>
    mutate(data_year = data_year,
           pm_frm_uncorrected = if_else(is.na(pm_236), pm_238, pm_236),
           pm_frm_corrected = if_else(is.na(pm_736), pm_738, pm_736),
           diff_corrected_uncorrected = pm_frm_corrected - pm_frm_uncorrected,
           diff_corrected_uncorrected_pct = (pm_frm_corrected - pm_frm_uncorrected)/pm_frm_uncorrected,
           pm_frm_uncorrected_over_0 = pm_frm_uncorrected > 9,  ## don't need to do this here!!
           pm_frm_corrected_over_0 = pm_frm_corrected > 9)
  
  
  monitor_data_filename <- paste0("monitor_data_", data_year, ".RDS")
  pm_data_filename <- paste0("pm_data_", data_year, ".RDS")
  
  saveRDS(monitor_data, file = here::here("data", "pm_downloads", "R_data_files",
                                       monitor_data_filename))
  saveRDS(pm_data, file = here::here("data", "pm_downloads", "R_data_files",
                                  pm_data_filename))
  
  inv_gc()

}

walk(all_unzips, process_raw_pm_data)
