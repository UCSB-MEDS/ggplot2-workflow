# data downloaded at: https://pasta.lternet.edu/package/eml/knb-lter-sbc/2007/16

#..........................load packages.........................
# library(EDIutils) # can use this pkg to download data directly from EDI
library(tidyverse)
library(chron)
library(naniar)

#..........................import data...........................
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
  
  # add month abbreviation
  mutate(month_name = as.factor(month.name[month])) |> 
  
  # replace 9999s with NAs
  replace_with_na(replace = list(Temp_bot = 9999, Temp_top = 9999, Temp_mid = 9999)) |> 
  
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


  
  