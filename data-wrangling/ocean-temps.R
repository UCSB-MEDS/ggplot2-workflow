# data downloaded at: https://pasta.lternet.edu/package/eml/knb-lter-sbc/2007/16

#..........................load packages.........................
# library(EDIutils) # can use this pkg to download data directly from EDI
library(tidyverse)
library(chron)
library(naniar)
library(ggridges)

#..........................read in data..........................
mko <- read_csv(here::here("raw-data", "mohawk_mooring_mko_20220330.csv"), col_names = TRUE, col_types = cols(.default = col_character()))

#..........................wrangle data..........................
mko_clean <- mko |> 
  
  # keep only necessary columns
  select(year, month, day, decimal_time, Temp_bot, Temp_top, Temp_mid) |> 
  
  # create date time column
  unite(date, year, month, day, sep = "-", remove = FALSE) |> 
  mutate(time = chron::times(as.numeric(decimal_time))) |> 
  unite(date_time, date, time, sep = " ") |> 
  
  # coerce data types
  mutate(date_time = as.POSIXct(date_time, "%Y-%m-%d %H:%M:%S", tz = "GMT"),
         year = as.factor(year),
         month = as.factor(month),
         day = as.numeric(day),
         Temp_top = as.numeric(Temp_top), 
         Temp_mid = as.numeric(Temp_mid), 
         Temp_bot = as.numeric(Temp_bot)) |> 
  
  # replace 9999s with NAs
  replace_with_na(replace = list(Temp_bot = 9999, Temp_top = 9999, Temp_mid = 9999)) |> 
  
  # filter years
  # filter(year %in% c(2016:2020)) |> 
  
  # add month abbreviation
  mutate(month_name = month.name[month]) |> 
  
  # select/reorder desired columns
  select(date_time, year, month, day, month_name, Temp_bot, Temp_mid, Temp_top) 

# saveRDS(mko_clean, here::here("clean-data", "mohawk_temps.rds"))

#......................explore missing data......................
# https://docs.google.com/presentation/d/1EzMU6be01aicPGeBcbXrXy1iX8CdWhBz1o2pJDPsXBM/edit#slide=id.p
# https://allisonhorst.shinyapps.io/missingexplorer/#section-introduction

# counts and percentage of missing data by year
see_NAs <- mko_clean %>% 
  group_by(year) %>% 
  naniar::miss_var_summary() |> 
  filter(variable == "Temp_bot")

# visualize missing Temp_bot 
bottom <- mko_clean |> select(Temp_bot)
naniar::vis_miss(bottom)

#.........................summarize data.........................
set.seed(12345) 
random_mko_sample <- dplyr::sample_n(mko_clean, 10)

#.......................calculate avg temp.......................
mko_avg <- mko_clean |> 
  summarize(mean_temp = mean(Temp_bot, na.rm = TRUE)) |> 
  pull()

#..............................plot..............................

# mko_ridges_plot <- 

mko_clean |> 
  group_by(month_name) |> 
  
  # create density ridges plot
  ggplot(aes(x = Temp_bot, y = month_name, fill = after_stat(x))) +
  ggridges::geom_density_ridges_gradient(rel_min_height = 0.01, scale = 3) + # rel_min_height cuts trailing tails (0.01 suggested); scale sets extent of overlap
  
  # set x axis breaks
  scale_x_continuous(breaks = c(9, 12, 15, 18, 21)) +
  
  # arrange months in reverse chronological order
  scale_y_discrete(limits = rev(month.name)) + 
  
  # fill color (palette or custom options)
  # scale_fill_viridis_c(name = "Temp. (°C)", option = "C") +
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

  
  
  



  
