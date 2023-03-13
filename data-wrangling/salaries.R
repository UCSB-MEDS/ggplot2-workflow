# women in the workplace: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-03-05
# https://twitter.com/thomas_mock/status/1102576601760448512

#..........................load packages.........................
library(tidyverse)
library(cowplot)
library(scales)
library(ggtext)

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
  drop_na(total_earnings_female, total_earnings_female) 

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
  mutate(perc_group = fct_relevel(perc_group, "f75", "f50", "m75")) |> 
  drop_na(perc_group) |> 
  filter(occupation %in% select_occupations)

# saveRDS(jobs_for_plotting, here::here("clean-data", "select_jobs.rds"))

#..............................plot..............................

# create df for labels
plot_labels <- data.frame(
  label = c("Occupations that are 75%+ women", "Occupations that are 45-55% women", "Occupations that are 75%+ men"),
  perc_group = c("f75", "f50", "m75") #,
  # x = c(3, 3, 3),
  # y = c(110800, 113000, 114000)
)

# plot
earnings_plot <- ggplot(jobs_for_plotting) +
  
  # create dumbbells
  geom_segment(aes(x = reorder(occupation, avg_salary), xend = occupation, y = total_earnings_female, yend = total_earnings_male)) +
  geom_point(aes(x = occupation, y = total_earnings_male), color = "#CD93D8", size = 2.5) +
  geom_point(aes(x = occupation, y = total_earnings_female), color = "#6A1E99", size = 2.5) +
  
  # flip axes & facet wrap by group
  coord_flip() +
  facet_wrap(~perc_group, nrow = 3, scale = "free_y") +
  
  # add annotations
  # geom_label(data = plot_labels, mapping = aes(x = Inf, y = Inf, label = label),
  #            fill = "white", label.padding = unit(0.25, "lines"),
  #            hjust = 1.05, vjust = 5) +
  
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
    strip.text.x = element_blank(),
    panel.spacing = unit(1, "lines"),
    
    # title & subtitle
    plot.title.position = "plot", # left-align plot title with left-edge of plot, not y-axis (see: https://github.com/tidyverse/ggplot2/issues/3252)
    plot.title = element_markdown(), # # enable markdown text (ggtext)
    plot.subtitle = element_markdown(margin = margin(t = 0, r = 0, b = 10, l = 0)), # enable markdown text (ggtext) 
    plot.caption = element_markdown(margin = margin(t = 15, r = 0, b = 0, l = 0)), # enable markdown text (ggtext)  
    
    # axes
    axis.title = element_blank(),
    
  )
  
earnings_plot

