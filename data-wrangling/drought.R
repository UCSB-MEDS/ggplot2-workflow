# https://twitter.com/BlakeRobMills/status/1536908360896978944
# https://github.com/BlakeRMills/TidyTuesday/blob/main/2022/Droughts%20(14%20Jun%202022)/Droughts%20(14%20Jun%202022).R

#..........................load packages.........................
library(tidyverse)
library(lubridate)
library(geofacet)
library(cowplot)
library(showtext)

#..........................import data...........................
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
state <- data.frame(state_name = state.name, state_abb = state.abb)

#......................import google fonts.......................
font_add_google(name = "Alfa Slab One", family = "alfa")
font_add_google(name = "Sen", family = "sen")
showtext::showtext_auto()

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
                            levels = c("D4", "D3", "D2", "D1","D0",
                                       "W0", "W1", "W2", "W3", "W4"),
                            labels = c("Exceptional Drought", "Extreme Drought",
                                       "Severe Drought", "Moderate Drought", "Abnormally Dry",
                                       "Abnormally Wet", "Moderate Wet",
                                       "Severe Wet", "Extreme Wet", "Exceptional Wet"))) |>

  # select / reorder necessary columns
  select(date, year, state_name = state, condition, condition_long, value) |>

  # filter for years 2012 onward
  filter(year >= 2012)

# add state abbreviations
drought_clean <- full_join(drought_clean, state)

# saveRDS(drought_clean, here::here("clean-data", "us_drought.rds"))

#..........................plot just CA..........................

# read in clean data
# drought_clean <- readRDS(here::here("clean-data", "us_drought.rds"))

# filter for just CA
drought_ca <- drought_clean |> filter(state_abb == "CA")

# color palette
colors <- c("#4D1212", "#9A2828", "#DE5A2C", "#DE922C", "#DEC02C", 
            "#ABCAFA", "#77A3EA", "#5287DE", "#243CB9", "#152473")
            
# title
ca_title = "Drought vs. Wet Conditions Across California"

# subtitle
ca_subtitle = "Percent area experiencing drought versus wet conditions from 2012 to 2022"

# plot     
ca_plot <- ggplot(drought_ca) + 
  
  # create area chart & add horizontal lines
  geom_area(aes(x = date, y = value, fill = condition_long)) +
  geom_hline(yintercept = 100, color = "#9B2F2F", linetype = 2) +
  geom_hline(yintercept = 50, color = "#FF9861", linetype = 2) +
  geom_hline(yintercept = -50, color = "#B7D2FF", linetype = 2) +
  geom_hline(yintercept = -100, color = "#2F3F9B", linetype = 2) +
  
  # set x-axis breaks
  scale_x_date(date_labels = "'%y", date_breaks = "2 years") +
  
  # set colors
  scale_fill_manual(values = colors) +
  
  # labs & titles
  labs(y = "% area",
       title = ca_title,
       subtitle = ca_subtitle) +
  
  # set theme
  theme_classic() +
  
  # customize theme 
  theme(
    plot.title = element_text(family = "alfa", color = "#303030", size = 16, face = "bold"),
    plot.subtitle = element_text(family = "sen", color = "#303030", size = 13),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(family = "sen", color = "#303030"),
    legend.background = element_rect(fill = "#9B9B9B", color = "#9B9B9B"),
    plot.background = element_rect(fill = "#9B9B9B", color = "#9B9B9B"), # 292828
    panel.background = element_rect(fill = "#9B9B9B", color = "#9B9B9B"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) +
  
  # customize legend
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = FALSE,
                             #title = "Conditions", title.position = "bottom", title.hjust = 0.5,
                             label.position = "bottom", keywidth = 3)) 

ca_plot

#........................add annotations.........................

# annotated_ca_plot <- cowplot::ggdraw(ca_plot) +
#   cowplot::draw_text(x = 1, y = 0.84, color = "#9B2F2F", text = "50% Drought", family = "sen", size = 11, fontface = "bold")
# 
# annotated_ca_plot

#............................save plot...........................
# ggsave(filename = "ca_droughts.png", plot = annotated_ca_plot, path = "saved-plots", height = 30, width = 60, units = "px", limitsize = FALSE)

#........................plot all states.........................
# 
# # remove HI, AK, DC from grid
# mygrid <- us_state_grid1 |> 
#   filter(!code %in% c("DC", "HI", "AK"))
# 
# # color palette
# colors <- c("#DEC02C", "#DE922C",  "#DE5A2C", "#9A2828", "#4D1212", 
#             "#ABCAFA", "#77A3EA", "#5287DE", "#243CB9", "#152473")
# 
# # plot
# ggplot(drought_clean) + 
#   geom_area(aes(x = date, y = value, fill = condition_long)) +
#   facet_geo(~state, grid = mygrid) +
#   scale_fill_manual(values = colors)
