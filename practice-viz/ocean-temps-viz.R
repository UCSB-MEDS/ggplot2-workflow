#..........................load packages.........................
library(tidyverse)
library(ggridges)

#..........................read in data..........................
mko_clean <- readRDS(here::here("clean-data", "mohawk_temps.rds"))

#.......................calculate avg temp.......................
mko_avg <- mko_clean |> 
  summarize(mean_temp = round(mean(Temp_bot, na.rm = TRUE), 1)) |> 
  pull() 

#..............................plot..............................
mko_ridges_plot <- mko_clean |> 
  group_by(month_name) |> 
  
  # create density ridges plot
  ggplot(aes(x = Temp_bot, y = month_name, fill = after_stat(x))) +
  ggridges::geom_density_ridges_gradient(rel_min_height = 0.01, scale = 3) + # rel_min_height cuts trailing tails (0.01 suggested); scale sets extent of overlap
  
  # add vertical line at avg temp + annotation
  geom_vline(xintercept = mko_avg, linetype = "dashed", color = "black") +
  annotate(geom = "segment", x = 18, y = "April", xend = mko_avg, yend = "May",
           arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "label", x = 18, y = "April", label = paste0(" Avg Temp = ", mko_avg, "°C"), hjust = "left") +
  
  # set x-axis breaks
  scale_x_continuous(breaks = c(9, 12, 15, 18, 21)) +
  
  # arrange months in reverse chronological order
  scale_y_discrete(limits = rev(month.name)) + 
  
  # fill color (palette or custom options)
  scale_fill_gradientn(colors = c("#2C5374","#778798", "#ADD8E6", "#EF8080", "#8B3A3A"), name = "Temp. (°C)") +
  
  # update labs
  labs(x = "Bottom Temperature (°C)",
       title = "Bottom Temperatures at Mohawk Reef, Santa Barbara, CA",
       subtitle = "Temperatures (°C) aggregated by month from 2005 - 2022") +
  
  # set theme
  ggridges::theme_ridges(font_size = 13, grid = TRUE) +
  
  # theme options
  theme(
    axis.title.y = element_blank()
  )

mko_ridges_plot
