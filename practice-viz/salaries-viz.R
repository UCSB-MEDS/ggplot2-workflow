#..........................load packages.........................
library(tidyverse)
library(scales)
library(ggtext)

#..........................import data...........................
# use this dataset if you want to explore all occupations included in the original TidyTuesday dataset
# all_jobs <- readRDS(here::here("clean-data", "all_jobs.rds")) 
# filtered data for plotting (original data contains too many to plot at once)
# includes only 10 occupations from each of the following categories (occupations that are 75%+ women, 45-55% women, and 75%+ men)
select_jobs <- readRDS(here::here("clean-data", "select_jobs.rds")) 

#..............................plot..............................
# create df for labels
# plot_labels <- data.frame(
#   label = c("Occupations that are 75%+ female", "Occupations that are 45-55% female", "Occupations that are 75%+ male"),
#   perc_group = factor(c("f75", "f50", "m75"), levels = c("f75", "f50", "m75")),
#   x = c(9, 9, 9),
#   y = c(400, 8, 8)
# )

earnings_plot <- ggplot(select_jobs) +
  
  # create dumbbells
  geom_segment(aes(x = reorder(occupation, avg_salary), xend = occupation, y = total_earnings_female, yend = total_earnings_male)) +
  geom_point(aes(x = occupation, y = total_earnings_male), color = "#CD93D8", size = 2.5) +
  geom_point(aes(x = occupation, y = total_earnings_female), color = "#6A1E99", size = 2.5) +
  
  # flip axes & facet wrap by group
  coord_flip() +
  facet_wrap(~perc_group_label, nrow = 3, scale = "free_y") +
  
  # add annotations
  # geom_text(data = plot_labels, mapping = aes(x = x, y = y + 1, label = label), # x = -Inf, y = -Inf
  #           fontface = "bold", size = 3) + # hjust = -0.5, vjust = -0.5, fill = "white",
  # # geom_fit_text(data = plot_labels, mapping = aes(x = x, y = y, label = label),
  # #               place = "topleft", reflow = TRUE) +
  
  # axis breaks & $ labels
  scale_y_continuous(breaks = c(25000, 50000, 75000, 100000, 125000),
                     labels = scales::label_dollar(scale = 0.001, suffix = "k")) + 
  
  # title & subtitle (add styling)
  labs(title = "**Earnings by Occupation and Sex**",
       subtitle = "Median earnings of full-time <span style='color:#CD93D8'>**male**</span> versus <span style='color:#6A1E99'>**female**</span> workers by occupation in 2016",
       caption = "<span style='color:#919092'>*Data courtesy of TidyTuesday (March 5, 2019)*</span>") +
  
  # theme
  theme_minimal() +
  theme(
    
    # facet panels
    strip.background = element_blank(),
    strip.text.x = element_text(face = "bold", size = 11),
    
    # title & subtitle
    plot.title.position = "plot", # left-align plot title with left-edge of plot, not y-axis (see: https://github.com/tidyverse/ggplot2/issues/3252)
    plot.title = element_textbox_simple(size = 20, lineheight = 1, padding = margin(t = 0, r = 0, b = 5, l = 0)), #element_markdown(), # # enable markdown text (ggtext)
    plot.subtitle = element_textbox_simple(size = 13, lineheight = 1, padding = margin(t = 5, r = 0, b = 10, l = 0)), # enable markdown text (ggtext) 
    plot.caption = element_markdown(margin = margin(t = 15, r = 0, b = 0, l = 0)), # enable markdown text (ggtext)  
    
    # plot margins
    # plot.margin = unit(c(1,3,1,1), "lines"), 
    
    # axes
    axis.title = element_blank(),
    
  )

earnings_plot
