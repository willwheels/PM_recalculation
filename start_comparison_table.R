
library(tidytable)



pm_hourly_files <- list.files(here::here("data", "pm_downloads", "unzipped_files"),
                              full.names = TRUE)

pm_hourly_files <- stringr::str_subset(pm_hourly_files, ".csv$")


pm_data <- fread(pm_hourly_files[1])

pm_data <- pm_data |>
  janitor::clean_names() |>
  mutate()


236, 238, 736, 738
