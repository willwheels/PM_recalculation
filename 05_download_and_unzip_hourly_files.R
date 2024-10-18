
download_zip_files <- function(file_year) {
  
  print(file_year)
  
  base_file_url <- "https://aqs.epa.gov/aqsweb/airdata/"
  
  
  download_filename <- paste0("hourly_88101_", file_year, ".zip")
  download_file_url <- paste0(base_file_url, download_filename)
  
  if(!file.exists(here::here("data", "pm_downloads", "zip_files", download_filename))) {
    
    download.file(download_file_url, destfile = here::here("data", "pm_downloads", "zip_files", download_filename))
  }

}


file_years <- as.character(2017:2023)

purrr::walk(file_years, ~ download_zip_files(.x))


all_zips <- list.files(here::here("data", "pm_downloads", "zip_files"),
                       full.names = TRUE)


purrr::map(all_zips, ~ unzip(.x, exdir = here::here("data", "pm_downloads", "unzipped_files")))
