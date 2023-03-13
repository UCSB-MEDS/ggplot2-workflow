#..........................load packages.........................
library(tidyverse)
library(lubridate)

#..........................import data...........................
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
state <- data.frame(state_name = state.name, state_abb = state.abb)

#..........................wrangle data..........................
drought_clean <- drought |> 

  # format date
  mutate(DATE = str_remove(DATE, "d_"),
         date = ymd(DATE),
         year = year(date)) |>

  # capitalize state names & remove hyphens
  mutate(state = str_to_title(state),
         state = str_replace(state, "-", " ")) |>

  # see note here re: cumulative data (tidytuesday 2021-07-20: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-20/readme.md)
  mutate(D0 = D0 - D1,
         D1 = D1 - D2,
         D2 = D2 - D3,
         D3 = D3 - D4,
         W0 = W0 - W1,
         W1 = W1 - W2,
         W2 = W2 - W3,
         W3 = W3 - W4) |>

  # wide to long
  pivot_longer(cols = c(D0:D4, W0:W4), names_to = "condition", values_to = "perc_area") |>

  # add condition levels
  mutate(condition_long = factor(condition,
                            levels = c("D4", "D3", "D2", "D1","D0",
                                       "W0", "W1", "W2", "W3", "W4"),
                            labels = c("Exceptional Drought", "Extreme Drought",
                                       "Severe Drought", "Moderate Drought", "Abnormally Dry",
                                       "Exceptional Wet", "Extreme Wet", 
                                       "Severe Wet", "Moderate Wet", "Abnormally Wet"))) |>

  # select / reorder necessary columns
  select(date, year, state_name = state, condition, condition_long, perc_area) |>
  
  # filter for years of interest 
  filter(year >= 2012)

# add state abbreviations & recordr cols
drought_clean <- full_join(drought_clean, state) |> 
  select(date, year, state_abb, state_name, condition, condition_long, perc_area)

# saveRDS(drought_clean, here::here("clean-data", "us_drought.rds"))


