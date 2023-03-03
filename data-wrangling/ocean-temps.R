# data downloaded at: https://pasta.lternet.edu/package/eml/knb-lter-sbc/2007/16

#..........................load packages.........................
library(EDIutils)
library(tidyverse)
library(chron)
library(naniar)

#..........................read in data..........................
mko <- read_csv(here::here("raw", "mohawk_mooring_mko_20220330.csv"), col_names = TRUE, col_types = cols(.default = col_character()))

#..........................wrangle data..........................
mko_clean <- mko |> 
  
  # keep only necessary columns
  select(year, month, day, decimal_time, Temp_bot, Temp_top, Temp_mid) |> 
  
  # create date time column
  unite(date, year, month, day, sep = "-", remove = FALSE) |> 
  mutate(time = chron::times(as.numeric(decimal_time))) |> 
  unite(date_time, date, time, sep = " ") |> 
  
  # remove decimal_time
  select(-c(decimal_time)) |> 
  
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
  filter(year %in% c(2016:2020))

# saveRDS(mko_clean, here::here("clean-data", "mohawk_temps.rds"))
# test <- readRDS(here::here("clean-data", "mohawk_temps.rds"))

#......................explore missing data......................

# https://docs.google.com/presentation/d/1EzMU6be01aicPGeBcbXrXy1iX8CdWhBz1o2pJDPsXBM/edit#slide=id.p
# https://allisonhorst.shinyapps.io/missingexplorer/#section-introduction

# counts and proportion of missing data by variable
see_NAs <- mko_clean %>% 
  group_by(year) %>% 
  miss_var_summary() |> 
  filter(variable %in% c("Temp_bot", "Temp_top", "Temp_mid"))

temp_top <- see_NAs |> filter(variable == "Temp_top")
temp_mid <- see_NAs |> filter(variable == "Temp_mid")
temp_bottom <- see_NAs |> filter(variable == "Temp_bot")

# visualize missing data
bottom <- mko_clean |> select(Temp_bot)
vis_miss(bottom)

#.........................summarize data.........................


#..............................plot..............................




  
