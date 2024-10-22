
library(sf)
library(dplyr)

monitor_files <- list.files(here::here("data", "pm_downloads", "R_data_files"),
                            full.names = TRUE)
monitor_files <- stringr::str_subset(monitor_files, "monitor_data")

read_one_year_monitor_data <- function(filename) {
  
  print(filename)
  
  monitor_data <- readRDS(filename)
  
  monitor_data <- monitor_data |>
    select(identifier, state_code, latitude, longitude, datum)

  
}

all_monitors <- purrr::map(monitor_files, read_one_year_monitor_data)

all_monitors <- all_monitors |>
  bind_rows() |>
  distinct()

#all_monitors2 <- monitor_data_2023 |> select(identifier) |> distinct()

monitor_map_WGS84 <- all_monitors |>
  filter(datum == "WGS84") |>
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               crs = sf::st_crs("WGS84"))

monitor_map_NAD83 <- all_monitors |>
  filter(datum == "NAD83") |>
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               crs = sf::st_crs("NAD83")) |>
  sf::st_transform(crs = sf::st_crs("WGS84"))

monitor_map_sf <- bind_rows(monitor_map_WGS84, monitor_map_NAD83) 

state_fips <- tigris::fips_codes |>
  select(state, state_code) |>
  distinct()

monitor_map_sf <- monitor_map_sf |>
  mutate(state_code = as.character(state_code),  
         state_code = stringr::str_pad(state_code, width = 2, pad = "0", side = "left")) |>
  left_join(state_fips, by = c("state_code")) |>
  filter(state %in% c(state.abb, "DC", "PR")) |>
  tigris::shift_geometry(#geoid = "state",
    preserve_area = FALSE, position = "below")

us_states_sf <- tigris::states() |>
  filter(STUSPS %in% c(state.abb, "DC", "PR")) |>
  tigris::shift_geometry(preserve_area = FALSE, position = "below")

save(monitor_map_sf, file = here::here("data", "monitor_map_sf.Rda"))
save(us_states_sf, file = here::here("data", "us_states_sf.Rda"))
