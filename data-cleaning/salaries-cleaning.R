# women in the workplace: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-03-05
# https://twitter.com/thomas_mock/status/1102576601760448512

#..........................load packages.........................
library(tidyverse)

#..........................import data...........................
jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
# earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 
# employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv") 

#..........................wrangle data..........................
jobs_gender_clean <- jobs_gender |> 
  mutate(percent_male = 100 - percent_female,
         difference_earnings_male_female = total_earnings_male - total_earnings_female,
         avg_salary = ((total_earnings_male + total_earnings_female)/2)) |> 
  relocate(year, major_category, minor_category, occupation,
          total_workers, workers_male, workers_female,
          percent_male, percent_female,
          total_earnings, total_earnings_male, total_earnings_female, difference_earnings_male_female,
          wage_percent_of_male, avg_salary) |> 
  drop_na(total_earnings_female, total_earnings_female) |> 
  mutate(occupation = as_factor(occupation))

# identify NAs
see_NAs <- jobs_gender_clean %>% 
  naniar::miss_var_summary() 
  
# saveRDS(jobs_gender_clean, here::here("raw-data", "all_jobs.rds"))

#.......................pre-plot wrangling.......................

# select occupations for plotting ----
select_occupations_75perc_female <- c("Dancers and choreographers", "Librarians", "Elementary and middle school teachers",
                                      "Registered nurses", "Dietitians and nutritionists", "Occupational therapists",
                                      "Secretaries and administrative assistants", "Medical assistants", "Hairdressers, hairstylists, and cosmetologists", 
                                      "Veterinary assistants and laboratory animal caretakers")

select_occupations_45_55perc_female <- c("Legislators", "Natural sciences managers", "Medical scientists", 
                                         "Postsecondary teachers", "Designers", "Bartenders", "Real estate brokers and sales agents",
                                         "Pharmacists", "Insurance sales agents", "Editors")

select_occupations_75perc_male <- c("Chief executives", "Software developers, applications and systems software", "Mathematicians", 
                                    "Chemical engineers", "Civil engineers", "Conservation scientists and foresters",
                                    "Police and sheriff's patrol officers", "Chefs and head cooks",
                                    "Construction and building inspectors", "Aircraft pilots and flight engineers")

select_occupations <- c(select_occupations_75perc_female, select_occupations_45_55perc_female, select_occupations_75perc_male)

jobs_for_plotting <- jobs_gender_clean |> 
  filter(year == 2016) |> 
  mutate(perc_group = case_when(
    percent_female >= 75 ~ "f75",
    percent_female >= 45 & percent_female <= 55 ~ "f50",
    percent_male >= 75 ~"m75"
  )) |> 
  mutate(perc_group_label = case_when(
    percent_female >= 75 ~ "Occupations that are 75%+ female",
    percent_female >= 45 & percent_female <= 55 ~ "Occupations that are 45-55% female",
    percent_male >= 75 ~ "Occupations that are 75%+ male"
  )) |> 
  mutate(perc_group = fct_relevel(perc_group, "f75", "f50", "m75"),
         perc_group_label = fct_relevel(perc_group_label, "Occupations that are 75%+ female", "Occupations that are 45-55% female", "Occupations that are 75%+ male")) |> 
  drop_na(perc_group) |> 
  filter(occupation %in% select_occupations)

# saveRDS(jobs_for_plotting, here::here("clean-data", "select_jobs.rds"))

