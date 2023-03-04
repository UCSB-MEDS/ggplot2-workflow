# https://data.ca.gov/dataset/surface-water-fecal-indicator-bacteria-results/resource/7639446f-8c62-43d9-a526-8bc7952dd8bd

#..........................load packages.........................
library(tidyverse)

#..........................wrangle data..........................
wq_clean <- read_csv(here::here("raw-data", "safetoswim_2010-2020_2023-03-03.csv"))
