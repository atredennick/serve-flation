#______________________________________________________________________________#
# Script to read in data from the tennis_slam_pointbypoint repo and combine
# across slams and years.
#
# Data source: https://github.com/JeffSackmann/tennis_slam_pointbypoint
# Author: Andrew Tredennick
# Date created: 2024-11-18
#______________________________________________________________________________#



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(rvest)
library(arrow)



# List files --------------------------------------------------------------

github_url <- "https://github.com/JeffSackmann/tennis_slam_pointbypoint"
page <- read_html(github_url)

# get links to files ending in .csv and are not mixed, doubles, or match data
csv_files <- page |>
  html_nodes("a") |>
  html_attr("href") |>
  str_subset("\\.csv$") |>
  # exclude unwanted files
  str_subset("^(?!.*(mixed|doubles)).*$") |>
  unique()

# convert to raw file URLs
csv_files <- paste0("https://raw.githubusercontent.com", csv_files) 
csv_files <- str_replace(csv_files, "/blob/", "/")

all_files <- tibble(files = csv_files) |>
  mutate(type = case_when(
    str_detect(files, "matches") ~ "match",
    str_detect(files, "points") ~ "points",
    TRUE ~ NA_character_
  ))


# Download files and combine in data frame --------------------------------

read_data_match <- function(.fname) {
  read_csv(.fname, show_col_types = FALSE) |>
    dplyr::select(match_id,
                  year,
                  slam,
                  event_name,
                  player1,
                  player2,
                  court_name,
                  court_id) |>
    as_tibble()
}

read_data_points <- function(.fname) {
  read_csv(.fname, show_col_types = FALSE) |>
    dplyr::select(match_id,
                  PointServer,
                  P1FirstSrvIn,
                  P2FirstSrvIn,
                  Speed_MPH) |>
    as_tibble()
}

points_data <- all_files |>
  filter(type == "points") |>
  mutate(data = map(.x = files, .f = ~ read_data_points(.x))) |>
  dplyr::select(data) |>
  unnest(cols = everything()) |>
  nest(points = -c(match_id))

match_data <- all_files |>
  filter(type == "match") |>
  mutate(data = map(.x = files, .f = ~ read_data_match(.x))) |>
  dplyr::select(data) |>
  unnest(cols = everything()) |>
  nest(matches = -c(match_id))

serve_data <- points_data |>
  left_join(match_data, by = join_by(match_id)) |>
  unnest(cols = everything()) |>
  relocate(slam,
           event_name,
           player1,
           player2,
           court_name,
           court_id,
           .before = PointServer)

# add tour information: ATP (men) or WTA (women)
serve_data <- serve_data |>
  mutate(tour = case_when(
    event_name == "Men's Singles|event_MS" ~ "ATP",
    event_name == "Women's Singles|event_WS" ~ "WTA",
    TRUE ~ NA_character_
  )) |>
  dplyr::select(-event_name) |>
  relocate(tour, .after = slam)



# Save the data -----------------------------------------------------------

# save as parquet file to avoid size limitation on GitHub
output_file <- here("data", "serve_data.parquet")
write_parquet(serve_data, output_file)
