# https://twitter.com/BlakeRobMills/status/1536908360896978944

#..........................load packages.........................
library(tidyverse)
library(lubridate)
library(geofacet)
# library(MetBrewer)
# library(showtext)
# library(cowplot)

#..........................import data...........................
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
state <- data.frame(state_name = state.name, state_abb = state.abb)

# font_add_google(name = 'Maven Pro')
# font_add_google(name = 'Space Grotesk')
font_add_google(name = "Pacifico", family = "pacifico")
showtext::showtext_auto()


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

# filter for just CA
drought_ca <- drought_clean |> filter(state_abb == "CA")

# color palette
colors <- c("#4D1212", "#9A2828", "#DE5A2C", "#DE922C", "#DEC02C", 
            "#ABCAFA", "#77A3EA", "#5287DE", "#243CB9", "#152473")
            
# title
ca_title = "Drought vs. Wet Conditions Across California"

# subtitle
ca_subtitle = "Percent area experiencing drought versus wet conditions across California from 2012 to 2022."

# plot                     
ggplot(drought_ca) + 
  
  # create area chart & add horizontal lines
  geom_area(aes(x = date, y = value, fill = condition_long)) +
  geom_hline(yintercept = 50, color = "#FFCECE", linetype = 2) +
  geom_hline(yintercept = 100, color = "#FFA3A3", linetype = 2) +
  geom_hline(yintercept = -50, color = "#CED1FF", linetype = 2) +
  geom_hline(yintercept = -100, color = "#8E95FF", linetype = 2) +
  
  # set x-axis breaks
  scale_x_date(date_labels = "'%y") +
  
  # set colors
  scale_fill_manual(values = colors) +
  
  # labs & titles
  labs(y = "% area",
       title = ca_title,
       subtitle = ca_subtitle) + 
  
  # customize legend
  guides(fill = guide_legend(nrow = 1, byrow = FALSE, reverse = FALSE,
                             #title = "Conditions", title.position = "bottom", title.hjust = 0.5,
                             label.position = "bottom", keywidth = 3, vjust = -5)) +
  
  # set theme
  theme_void() +
  
  # customize theme 
  theme(
    plot.title = element_text(color = "#303030", face = "bold"),
    plot.subtitle = element_text(color = "#303030"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(color = "#303030"),
    legend.background = element_rect(fill = "#9B9B9B", color = "#9B9B9B"),
    plot.background = element_rect(fill = "#9B9B9B", color = "#9B9B9B"), # 292828
    panel.background = element_rect(fill = "#9B9B9B", color = "#9B9B9B"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) 
  


#........................plot all states.........................

# remove HI, AK, DC from grid
mygrid <- us_state_grid1 |> 
  filter(!code %in% c("DC", "HI", "AK"))

# color palette
colors <- c("#DEC02C", "#DE922C",  "#DE5A2C", "#9A2828", "#4D1212", 
            "#ABCAFA", "#77A3EA", "#5287DE", "#243CB9", "#152473")

# plot
ggplot(drought_clean) + 
  geom_area(aes(x = date, y = value, fill = condition_long)) +
  facet_geo(~state, grid = mygrid) +
  scale_fill_manual(values = colors)
