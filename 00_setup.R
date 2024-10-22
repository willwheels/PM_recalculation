## package list based on what's not on DMAP, could switch to renv if not on dmap

needed_packages <- c(#"RAQSAPI",
                     #"getPass", 
  "here", "tigris", 
  "tidytable", "janitor",
  "ggthemes")

purrr::walk(needed_packages, ~if(!.x %in% installed.packages()) {install.packages(.x)})


if(!dir.exists(here::here("data"))) {dir.create(here::here("data"))}
if(!dir.exists(here::here("data", "pm_obs"))) {dir.create(here::here("data", "pm_obs"))}
if(!dir.exists(here::here("data", "pm_downloads", "zip_files"))) {
  
  dir.create(here::here("data", "pm_downloads", "zip_files", "annual"), recursive = TRUE)
  
}

if(!dir.exists(here::here("data", "pm_downloads", "unzipped_files"))) {
  
  dir.create(here::here("data", "pm_downloads", "unzipped_files"), recursive = TRUE)
  
}

if(!dir.exists(here::here("data", "pm_downloads", "R_data_files"))) {
  
  dir.create(here::here("data", "pm_downloads", "R_data_files"), recursive = TRUE)
  
}

