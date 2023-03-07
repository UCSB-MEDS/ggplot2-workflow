
# #..........................load packages.........................
library(tidyverse)
library(lubridate)
library(geofacet)
# library(MetBrewer)
library(showtext)
# library(cowplot)

#..........................import data...........................
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
state <- data.frame(state_name = state.name, state_abb = state.abb)

font_add_google(name = 'Maven Pro')
font_add_google(name = 'Space Grotesk')


# # Aes
# cols <- c(met.brewer("Hiroshige")[1:5],
#           met.brewer("Hiroshige")[6:10] %>% rev())
# showtext_auto()
# font_add_google("Advent Pro")

#..........................wrangle data..........................
drought_clean <- drought %>% 
  
  # format dates
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
  pivot_longer(cols = c(D0:D4, W0:W4), names_to = "condition", values_to = "value") |> 
  
  # make wet condition values negative by multiplying by -1
  mutate(value = ifelse(grepl("D", condition) == T, value, value * -1)) |>
  # mutate(wet = str_detect(condition, "W", negate = FALSE), value, value * -1) |> 
  # mutate(value_adjusted = case_when(wet == TRUE ~ value * -1)) |> # doesn't work
  
  # add condition levels
  mutate(condition_long = factor(condition,
                            levels = c("D4", "D3", "D2", "D1", "D0",
                                       "W4", "W3", "W2", "W1", "W0"),
                            labels = c("Exceptional Drought", "Extreme Drought", 
                                       "Severe Drought", "Moderate Drought", 
                                       "Abnormally Dry", "Exceptional Wet", 
                                       "Extreme Wet", "Severe Wet", 
                                       "Moderate Wet", "Abnormally Wet"))) |> 
  
  # select / reorder necessary columns
  select(date, year, state, condition, condition_long, value) |> 
  
  # filter for years 2012 onward
  filter(year >= 2012)

mygrid <- us_state_grid1 |> 
  filter(!code %in% c("DC", "HI", "AK"))

#..............................plot..............................
colors <- c("#4D1212", "#9A2828", "#DE5A2C", "#DE922C", "#DEC02C",
            "#152473", "#243CB9", "#5287DE", "#77A3EA", "#ABCAFA")

ggplot(drought_clean) + 
  geom_area(aes(x = date, y = value, fill = condition_long)) +
  facet_geo(~state, grid = mygrid) +
  scale_fill_manual(values = colors)
