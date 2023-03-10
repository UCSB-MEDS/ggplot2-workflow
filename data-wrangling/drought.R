#..........................load packages.........................
library(tidyverse)
library(lubridate)
library(cowplot)
library(showtext)
library(geofacet)

#..........................import data...........................
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
state <- data.frame(state_name = state.name, state_abb = state.abb)

#......................import google fonts.......................

# {showtext}: https://github.com/yixuan/showtext

# load Google Fonts: https://fonts.google.com/ ----
font_add_google(name = "Alfa Slab One", family = "alfa")
font_add_google(name = "Sen", family = "sen")

# automatically use {showtext} to render text for future devices ----
showtext::showtext_auto()

# tell showtext the resolution for the device ----
showtext_opts(dpi = 300)

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

#..........................plot just CA..........................

# read in clean data ----
drought_clean <- readRDS(here::here("clean-data", "us_drought.rds"))

# color palette ----
colors <- c("#4D1212", "#9A2828", "#DE5A2C", "#DE922C", "#DEC02C", 
            "#152473", "#243CB9", "#5287DE", "#77A3EA", "#ABCAFA")
            
# plot ----     
ca_plot <- drought_clean |>
  
  # filter for CA ----
 filter(state_abb == "CA") |> 
  
  # make wet condition values negative by multiplying by -1 ----
  mutate(perc_area = ifelse(test = grepl("D", condition) == T, yes = perc_area, no = perc_area * -1)) |>
  
  # initialize ggplot ----
  ggplot(aes(x = date, y = perc_area, fill = condition_long)) + 
  
  # create stacked area chart & add horizontal lines ----
  geom_area() +
  geom_hline(yintercept = 100, color = "#303030", alpha = 0.55, linetype = 2) +
  geom_hline(yintercept = 50, color = "#5B5B5B", alpha = 0.55, linetype = 2) +
  geom_hline(yintercept = -50, color = "#5B5B5B", alpha = 0.55, linetype = 2) +
  geom_hline(yintercept = -100, color = "#303030", alpha = 0.55, linetype = 2) +
  
  # set x-axis breaks ----
  # scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  
  # set colors ----
  scale_fill_manual(values = colors) +
  
  # labs & titles ----
  labs(y = "% area",
       title = "Drought vs. Wet Conditions Across California",
       subtitle = "Percent area experiencing drought versus wet conditions from 2012 to 2022") +
  
  # set theme ----
  theme_classic() +
  
  # customize theme ----
  theme(
    
    # background colors 
    plot.background = element_rect(fill = "#9B9B9B", color = "#9B9B9B"), 
    panel.background = element_rect(fill = "#9B9B9B", color = "#9B9B9B"),
    
    # titles 
    plot.title = element_text(family = "alfa", color = "#303030", size = 17, margin = margin(t = 5, r = 10, b = 5, l = 0)),
    plot.subtitle = element_text(family = "sen", color = "#303030", size = 14, margin = margin(t = 0, r = 10, b = 20, l = 0)),
    
    # legend 
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(family = "sen", color = "#303030", size = 9),
    legend.background = element_rect(fill = "#9B9B9B", color = "#9B9B9B"),
    legend.key.width = unit(4, 'cm'),
    
    # axes 
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) +
  
  # horizontal legend ----
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = FALSE,
                             label.position = "bottom")) 

ca_plot

#........................add annotations.........................

annotated_ca_plot <- cowplot::ggdraw(ca_plot) +
  cowplot::draw_text(x = 0.95, y = 0.88, color = "#303030", text = "100% Drought", family = "sen", size = 11, fontface = "bold") + # x = 0.94, y = 0.85 (RStudio Viewer) | x = 0.95, y = 0.88 (ggsave)
  cowplot::draw_text(x = 0.955, y = 0.725, color = "#5B5B5B", text = "50% Drought", family = "sen", size = 11, fontface = "bold") + # x = 0.94, y = 0.72 (RStudio Viewer) | x = 0.955, y = 0.725 (ggsave)
  cowplot::draw_text(x = 0.965, y = 0.415, color = "#5B5B5B", text = "50% Wet", family = "sen", size = 11, fontface = "bold") + # x = 0.955, y = 0.455 (RStudio Viewer) | x = 0.965, y = 0.415 (ggsave)
  cowplot::draw_text(x = 0.965, y = 0.26, color = "#303030", text = "100% Wet", family = "sen", size = 11, fontface = "bold") # x = 0.955, y = 0.32 (RStudio Viewer) | x = 0.965, y = 0.26 (ggsave)

annotated_ca_plot

#............................save plot...........................
# ggsave(filename = "ca_droughts.png", plot = annotated_ca_plot, path = "saved-plots")

#........................plot all states.........................

# remove HI, AK, DC from grid ----
mygrid <- us_state_grid1 |>
  filter(!code %in% c("DC", "HI", "AK"))

# color palette ----
colors <- c("#4D1212", "#9A2828", "#DE5A2C", "#DE922C", "#DEC02C", 
            "#152473", "#243CB9", "#5287DE", "#77A3EA", "#ABCAFA")

# plot ----
drought_clean |> 
  
  # make wet condition values negative by multiplying by -1 ----
  mutate(perc_area = ifelse(test = grepl("D", condition) == T, yes = perc_area, no = perc_area * -1)) |>
  
  # plot ----
  ggplot() +
  geom_area(aes(x = date, y = perc_area, fill = condition_long)) +
  facet_geo(~state_abb, grid = mygrid) +
  scale_fill_manual(values = colors)
