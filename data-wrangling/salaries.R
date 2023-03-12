# women in the workplace: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-03-05
# https://twitter.com/thomas_mock/status/1102576601760448512
#..........................load packages.........................
library(tidyverse)
library(patchwork)

#..........................import data...........................
jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
# earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 
# employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv") 

#..........................wrangle data..........................
jobs_gender_clean <- jobs_gender |> 
  mutate(percent_male = 100 - percent_female,
         difference_earnings_male_female = total_earnings_male - total_earnings_female) |> 
  relocate(year, major_category, minor_category, occupation,
          total_workers, workers_male, workers_female,
          percent_male, percent_female,
          total_earnings, total_earnings_male, total_earnings_female, difference_earnings_male_female,
          wage_percent_of_male) |> 
  drop_na(total_earnings_female, total_earnings_female) |> 
  mutate(avg_salary = ((total_earnings_male + total_earnings_female)/2))

# identify NAs
see_NAs <- jobs_gender_clean %>% 
  naniar::miss_var_summary() 
  
# saveRDS(jobs_gender_clean, here::here("raw-data", "jobs.rds"))

#.......................pre-plot wrangling.......................


######################## female 75% #########################

# filter for year 2016 & occupations that are >=75% female ----
female_75perc <- jobs_gender_clean |> 
  filter(year == "2016") |> 
  filter(percent_female >= 75) #|>
  # slice_sample(n = 16) |> 

# get 16 random jobs
# set.seed(123)
# random_occupations_75perc_male <- sample_n(female_75perc, 16)

# select occupations that are >= 75% female ----
select_occupations_75perc_female <- c("Dancers and choreographers", "Librarians", "Elementary and middle school teachers",
                        "Registered nurses", "Dietitians and nutritionists", "Occupational therapists",
                        "Compensation, benefits, and job analysis specialists",
                        "Secretaries and administrative assistants",
                        "Medical assistants", "Hairdressers, hairstylists, and cosmetologists", "Veterinary assistants and laboratory animal caretakers", "Hosts and hostesses, restaurant, lounge, and coffee shop")

female_75perc_select_occupations <- female_75perc |> 
  filter(occupation %in% select_occupations_75perc_female) |> 
  # mutate(avg_salary = ((total_earnings_male + total_earnings_female)/2)) |> 
  arrange(-avg_salary)

######################## 45-55% female #########################

# filter for year 2016 & occupations that are >=75% male ----
female_45_55perc <- jobs_gender_clean |> 
  filter(year == "2016") |> 
  filter(percent_female >= 45 & percent_female <= 55) #|>
# slice_sample(n = 16) |> 

# get 16 random jobs
# set.seed(123)
# random_occupations_75perc_male <- sample_n(male_75perc, 16)

# select occupations that are >= 75% female ----
select_occupations_45_55perc_female <- c("Legislators", "Natural sciences managers", "Medical scientists", 
                                         "Postsecondary teachers", "Designers", "Bartenders", "Real estate brokers and sales agents",
                                         "Pharmacists", "Credit analysts", "Animal trainers", "Insurance sales agents", 
                                         "Editors")

female_45_55perc_select_occupations <- female_45_55perc |> 
  filter(occupation %in% select_occupations_45_55perc_female) |> 
  # mutate(avg_salary = ((total_earnings_male + total_earnings_female)/2)) |> 
  arrange(-avg_salary)

######################## male 75% #########################

# filter for year 2016 & occupations that are >=75% male ----
male_75perc <- jobs_gender_clean |> 
  filter(year == "2016") |> 
  filter(percent_male >= 75) #|>
# slice_sample(n = 16) |> 

# get 16 random jobs
# set.seed(123)
# random_occupations_75perc_male <- sample_n(male_75perc, 16)

# select occupations that are >= 75% female ----
select_occupations_75perc_male <- c("Chief executives",
                               "Software developers, applications and systems software", "Mathematicians", "Chemical engineers", "Civil engineers", "Conservation scientists and foresters",
                               "Police and sheriff's patrol officers", "Chefs and head cooks",
                               "Construction and building inspectors", "Precision instrument and equipment repairers", "Electricians",
                               "Aircraft pilots and flight engineers")

male_75perc_select_occupations <- male_75perc |> 
  filter(occupation %in% select_occupations_75perc_male) |> 
  # mutate(avg_salary = ((total_earnings_male + total_earnings_female)/2)) |> 
  arrange(-avg_salary)

#..............................plot..............................
female75_plot <- ggplot(female_75perc_select_occupations) +
  geom_segment(aes(x = reorder(occupation, avg_salary), xend = occupation, y = total_earnings_female, yend = total_earnings_male)) +
  geom_point(aes(x = occupation, y = total_earnings_male), color = "#CD93D8", size = 2.5) +
  geom_point(aes(x = occupation, y = total_earnings_female), color = "#6A1E99", fill = "#6A1E99", size = 2.5, shape = 23) +
  coord_flip() + 
  theme(
    legend.position = "none"
  )

female75_plot

female45_55_plot <- ggplot(female_45_55perc_select_occupations) +
  geom_segment(aes(x = reorder(occupation, avg_salary), xend = occupation, y = total_earnings_female, yend = total_earnings_male)) +
  geom_point(aes(x = occupation, y = total_earnings_male), color = "#CD93D8", size = 2.5) +
  geom_point(aes(x = occupation, y = total_earnings_female), color = "#6A1E99", fill = "#6A1E99", size = 2.5, shape = 23) +
  coord_flip() +
  theme(
    legend.position = "none"
  )

female45_55_plot

male75_plot <- ggplot(male_75perc_select_occupations) +
  geom_segment(aes(x = reorder(occupation, avg_salary), xend = occupation, y = total_earnings_female, yend = total_earnings_male)) +
  geom_point(aes(x = occupation, y = total_earnings_male), color = "#CD93D8", size = 2.5) +
  geom_point(aes(x = occupation, y = total_earnings_female), color = "#6A1E99", fill = "#6A1E99", size = 2.5, shape = 23) +
  coord_flip() +
  theme(
    legend.position = "none"
  )

male75_plot

female75_plot + female45_55_plot + male75_plot + plot_layout(nrow = 3, byrow = FALSE)
