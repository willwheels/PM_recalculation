
library(tidytable)
library(ggplot2)

## note that I discovered a minor error--a handful of  monitors have >1 poc and not all calculations account for that

pm_data_2023 <- readRDS(here::here("data", "pm_downloads", "R_data_files", "pm_data_2023.RDS"))

pm_data_2023_summ <- pm_data_2023 |>
  group_by(identifier, latitude, longitude, datum, state_code) |>
  summarize(mean_uncorrected = mean(pm_frm_uncorrected, na.rm = TRUE),
            mean_corrected = mean(pm_frm_corrected, na.rm = TRUE),
            mean_change = mean(diff_corrected_uncorrected, na.rm = TRUE),
            mean_pct_change = mean(diff_corrected_uncorrected_pct, na.rm = TRUE),) |>
  ungroup() |>
  mutate(uncorrected_gt_9 = mean_uncorrected > 9,
         corrected_gt_9 = mean_corrected > 9)

summary(pm_data_2023_summ) 

num_switches = sum(pm_data_2023_summ$mean_uncorrected > 9  & pm_data_2023_summ$mean_corrected < 9, na.rm = TRUE)

print("oh no, the number of monitors that switch from above to below the naaqs is ")
print(num_switches)

switched_monitors <- pm_data_2023_summ |>
  filter(uncorrected_gt_9 == TRUE & corrected_gt_9 == FALSE)

## cut and paste
## need to put switch_geometry trick back in--create a separate saved data file and reuse that
## check using correct variable with new pipeline

monitor_map_WGS84_2023 <-pm_data_2023_summ |>
  filter(datum == "WGS84") |>
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               crs = sf::st_crs("WGS84"))

monitor_map_NAD83_2023 <- pm_data_2023_summ |>
  filter(datum == "NAD83") |>
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               crs = sf::st_crs("NAD83")) |>
  sf::st_transform(crs = sf::st_crs("WGS84"))

monitor_map_sf_2023 <- bind_rows(monitor_map_WGS84_2023, monitor_map_NAD83_2023) 

state_fips <- tigris::fips_codes |>
  select(state, state_code) |>
  distinct()


monitor_map_sf_2023 <- monitor_map_sf_2023 |>
  dplyr::mutate(state_code = as.character(state_code),  ## have to track down when this got messed up
         state_code = stringr::str_pad(state_code, width = 2, pad = "0", side = "left")) |>
  dplyr::left_join(state_fips, by = c("state_code"))

ggplot(monitor_map_sf_2023 |> dplyr::filter(!state %in% c("AK", "HI")), 
       aes(color = mean_change)) +
  geom_sf() +
  stat_sf_coordinates() +
  labs(title = "Map of PM Corrected Monitors in 2023, Average Change in Hourly PM 2.5") +
  ggthemes::theme_map() +  ## theme covers up hawaii!!!
  scale_color_viridis_c()

ggplot(monitor_map_sf_2023 |> dplyr::filter(!state %in% c("AK", "HI")), 
       aes(color = mean_pct_change)) +
  geom_sf() +
  stat_sf_coordinates() +
  labs(title = "Map of PM Corrected Monitors in 2023, Average % Change in Hourly PM 2.5") +
  ggthemes::theme_map() +  ## theme covers up hawaii!!!
  scale_color_viridis_c()
