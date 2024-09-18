# Homework Week 4:
# 1) calculates the mean and variance of body mass by species, 
# island, and sex without any NAs.

# 2) filters out (i.e. excludes) male penguins, 
# then calculates the log body mass,
# then selects only the columns for species, 
# island, sex, and log body mass, then use these data to make any plot. 

# Created by: Keanu Rochette-Yu Tsuen
# Date: 2024-09-17

### Load Libraries #######
library(tidyverse)
library(here)
library(palmerpenguins)

### Load Data #######
glimpse(penguins)

### practicing with dplyr ########
penguins %>% 
  drop_na(species, island, sex) %>% # na.omit() also an option
  group_by(species, island, sex) %>% 
  summarise(mean_mass_g = mean(body_mass_g),
            variance = var(body_mass_g))

### platyin with dplyr and ggolot ########
penguins %>% 
  filter(sex !="male") %>% 
  mutate(log_mass = log(body_mass_g)) %>% 
  select(species, island, sex, log_mass) %>% 
  ggplot(aes(x= 0, y=log_mass))+
  geom_col()




