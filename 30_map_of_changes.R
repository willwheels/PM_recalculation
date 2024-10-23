
library(tidytable)
library(ggplot2)
library(sf) ## call to prevent error in joining sf to tibble

## note that I discovered a minor error--a handful of  monitors have >1 poc and not all calculations account for that
## should probably move creation of change df to another file

pm_data_2023 <- readRDS(here::here("data", "pm_downloads", "R_data_files", "pm_data_2023.RDS"))

pm_data_2023_changes <- pm_data_2023 |>
  group_by(identifier, latitude, longitude, datum, state_code) |>
  summarize(mean_uncorrected = mean(pm_frm_uncorrected, na.rm = TRUE),
            mean_corrected = mean(pm_frm_corrected, na.rm = TRUE),
            mean_change = mean(diff_corrected_uncorrected, na.rm = TRUE),
            mean_pct_change = mean(diff_corrected_uncorrected_pct, na.rm = TRUE),) |>
  ungroup() |>
  mutate(uncorrected_gt_9 = mean_uncorrected > 9,
         corrected_gt_9 = mean_corrected > 9)

summary(pm_data_2023_changes) 

num_switches = sum(pm_data_2023_changes$mean_uncorrected > 9  & pm_data_2023_changes$mean_corrected < 9, na.rm = TRUE)

print("oh no, the number of monitors that switch from above to below the naaqs is ")
print(num_switches)

switched_monitors <- pm_data_2023_changes |>
  filter(uncorrected_gt_9 == TRUE & corrected_gt_9 == FALSE)

load(here::here("data", "monitor_map_sf.Rda"))
load(here::here("data", "us_states_sf.Rda"))

pm_data_2023_changes <- pm_data_2023_changes |>
  dplyr::mutate(state_code = as.character(state_code),  ## have to track down when this got messed up
                state_code = stringr::str_pad(state_code, width = 2, pad = "0", side = "left"))


monitor_map_sf_2023 <- monitor_map_sf |>
  dplyr::left_join(pm_data_2023_changes)
  

ggplot(monitor_map_sf_2023) +
  geom_sf(aes(color = mean_change)) +
  geom_sf(data = us_states_sf, fill = NA) +
  labs(title = "Map of PM Corrected Monitors in 2023, Average Change in Hourly PM 2.5") +
  ggthemes::theme_map() + 
  scale_color_viridis_c()

ggsave(here::here("figs", "monitor_map_pm_changes.png"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(monitor_map_sf_2023) +
  geom_sf(aes(color = mean_pct_change)) +
  geom_sf(data = us_states_sf, fill = NA) +
  labs(title = "Map of PM Corrected Monitors in 2023, Average % Change in Hourly PM 2.5") +
  ggthemes::theme_map() +  ## theme covers up hawaii!!!
  scale_color_viridis_c()
