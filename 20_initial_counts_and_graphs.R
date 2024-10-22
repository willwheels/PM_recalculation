library(tidyverse) ## need to be careful not to use tidytable w/ sf
library(ggplot2)

## rewrite to use named vectors to define monitor categories
## make colors consistent across graphs and/or direct label them


all_data_files <- list.files(here::here("data", "pm_downloads", "R_data_files"),
                             full.names = TRUE)


monitor_files <- stringr::str_subset(all_data_files, "monitor")
pm_data_files <- stringr::str_subset(all_data_files, "pm_data")

all_monitor_data <- purrr::map(monitor_files, readRDS)

all_monitor_data <- all_monitor_data |>
  bind_rows() |>
  ungroup()


should_be_corrected_counts <- all_monitor_data |>
  filter(method_code %in% c(236, 238)) |>
  group_by(data_year) |>
  summarise(annual_count = sum(n)) |>
  ungroup() |>
  mutate(data_type = "correction needed")
  
corrected_counts <- all_monitor_data |>
  filter(method_code %in% c(736, 738)) |>
  group_by(data_year) |>
  summarise(annual_count = sum(n)) |>
  ungroup() |>
  mutate(data_type = "corrected")

uncorrected_counts <- all_monitor_data |>
  filter(!method_code %in% c(236, 238, 736, 738, 636, 638)) |>
  group_by(data_year) |>
  summarise(annual_count = sum(n)) |>
  ungroup() |>
  mutate(data_type = "correction not needed")

aligned_counts <- all_monitor_data |>
  filter(method_code %in% c(636, 638)) |>
  group_by(data_year) |>
  summarise(annual_count = sum(n)) |>
  ungroup() |>
  mutate(data_type = "aligned")

all_counts <- bind_rows(should_be_corrected_counts, corrected_counts, uncorrected_counts, aligned_counts) |>
  arrange(data_year, data_type)

total_counts <- all_counts |>
  filter(data_type %in% c("correction needed", "correction not needed", "aligned")) |>
  group_by(data_year) |>
  summarise(annual_count = sum(annual_count)) |>
  ungroup() |>
  mutate(data_type = "Total")

all_counts <- all_counts |>
  bind_rows(total_counts) |>
  arrange(data_year, data_type)


## should probably make Total black
## end graph in 2023


ggplot(data = all_counts |> 
         filter(data_year < 2024, data_type %in% c("correction needed", "correction not needed", "aligned", "Total")), 
       aes(x = data_year, y = annual_count, group = data_type, color = data_type)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  ggthemes::scale_color_colorblind()

## note--should I redo this by month?

## This does not account well for monitors that shifted to aligned 

## same but just monitors

should_be_corrected_monitors <- all_monitor_data |>
  filter(method_code %in% c(236, 238)) |>
  group_by(data_year) |>
  summarise(annual_monitors = n()) |>
  ungroup() |>
  mutate(data_type = "correction needed")

corrected_monitors <- all_monitor_data |>
  filter(method_code %in% c(736, 738)) |>
  group_by(data_year) |>
  summarise(annual_monitors = n()) |>
  ungroup() |>
  mutate(data_type = "corrected")

uncorrected_monitors <- all_monitor_data |>
  filter(!method_code %in% c(236, 238, 736, 738, 636, 638)) |>
  group_by(data_year) |>
  summarise(annual_monitors = n()) |>
  ungroup() |>
  mutate(data_type = "correction not needed")

aligned_monitors <- all_monitor_data |>
  filter(method_code %in% c(636, 638)) |>
  group_by(data_year) |>
  summarise(annual_monitors = n()) |>
  ungroup() |>
  mutate(data_type = "aligned")

all_monitors <- bind_rows(corrected_monitors, uncorrected_monitors, aligned_monitors)

total_monitors <- all_monitors |>
  filter(data_type %in% c("corrected", "correction not needed")) |>
  group_by(data_year) |>
  summarise(annual_monitors = sum(annual_monitors)) |>
  ungroup() |>
  mutate(data_type = "Total")

all_monitors <- all_monitors |>
  bind_rows(total_monitors) |>
  arrange(data_year, data_type)

ggplot(data = all_monitors|> 
         filter(data_year < 2024, data_type %in% c("corrected", "correction not needed", "Total")), 
       aes(x = data_year, y = annual_monitors, group = data_type, color = data_type)) +
  geom_line() +
  geom_point() +
  labs(caption = "Aligned observations dropped to avoid double counting") +
  theme_minimal() +
  ggthemes::scale_color_colorblind()


###


load(here::here("data", "monitor_map_sf.Rda"))
load(here::here("data", "us_states_sf.Rda"))

monitor_data_2023 <- readRDS(here::here("data", "pm_downloads", "R_data_files", "monitor_data_2023.RDS"))

monitor_data_2023 <- monitor_data_2023 |>
  group_by(identifier) |>
  mutate(corrected = n() > 1) |>
  ungroup() |>
  select(-poc, -method_code, -n) |>
  distinct() |>
  select(identifier, corrected)

monitor_map_sf_2023 <- monitor_map_sf |>
  left_join(monitor_data_2023) |>
  filter(!is.na(corrected))

ggplot(monitor_map_sf_2023) +
  geom_sf(aes(color = corrected)) +
  geom_sf(data = us_states, fill = NA) +
  #stat_sf_coordinates() +
  labs(title = "Map of PM monitors in 2023",
       caption = "AK, HI, and PR repositioned and not to scale") +
  ggthemes::theme_map() +  
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "right")
  
### eliminate 2017 monitors

monitor_data_2017 <- readRDS(here::here("data", "pm_downloads", "R_data_files", "monitor_data_2017.RDS" )) 


monitors_2017 <- monitor_data_2017$identifier

new_monitors_since_2017 <- monitor_map_sf_2023 |>
  #filter(!identifier %in% monitor_data_2017$identifier) ## tidytable::filter tossing error
  dplyr::filter(!identifier %in% monitors_2017 )


ggplot(new_monitors_since_2017) +
  geom_sf(aes(color = corrected)) +
  geom_sf(data = us_states, fill = NA) +
  #stat_sf_coordinates() +
  labs(title = "Map of PM monitors in 2023 Since 2017",
       caption = "AK, HI, and PR repositioned and not to scale") +
  ggthemes::theme_map() +  ## theme covers up hawaii!!!
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "right")
  
  

  