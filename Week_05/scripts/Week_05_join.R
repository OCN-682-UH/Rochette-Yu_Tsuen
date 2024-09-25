### Learn bout the join family of functions ######
### Created by: Keanu Rochette-Yu Tsuen
### Date: 2024-09-24

### Libraries ##########
library(tidyverse)
library(here)

### Load data #######
# Thermal performance data 
TPCdata <- read.csv(here("Week_05", "data", "Topt_data.csv"))
glimpse(TPCdata)
# Environmental Data from each site
envirodata<- read.csv(here("Week_05", "data", "site.characteristics.data.csv"))
glimpse(envirodata)

#Pivot data to wide format and sort out the column order
envirodata_wide <- envirodata %>% 
  pivot_wider(names_from = parameter.measured,
              values_from = values) %>% 
  arrange(site.letter) #arrange the df by site
view(envirodata_wide)

### left_join() #######
# Careful ! Deletes the data from the df 
# on the right if no data matches the left df

full_data_left <- left_join(TPCdata, envirodata_wide)
#Joining with `by = join_by(site.letter)`
view(full_data_left)

#### relocate() #######
full_data_left <- left_join(TPCdata, envirodata_wide) %>% 
  relocate(where(is.numeric), .after=where(is.character))

#### Exercise #######
# summarize by site and by parameters

# Using pivot_longer(), aka the long way
full_data_left %>% pivot_longer(cols = c(E:substrate.cover, -site.block), 
                                names_to = "parameters",
                                values_to = "values") %>% 
  group_by(site.letter, parameters) %>% 
  drop_na() %>% 
  summarise(mean_val = mean(values),
            var_val = var(values)) 

# Using summarize_at(), aka the short way
full_data_left %>%
  group_by(site.letter) %>% 
  summarise_at(vars(c(E:substrate.cover, -site.block)), 
               list(mean_val= mean, var_val = var)) 
              # also funs(mean_val = mean, var_val = var)

# Using summarize(across()), another way
full_data_left %>%
  group_by(site.letter) %>% 
  summarise(across(where(is.numeric), 
            list(mean_val= mean, var_val = var), na.rm = T))

# Using summarize_if(), another way
full_data_left %>%
  group_by(site.letter) %>% 
  summarise_if(is.numeric, 
               list(mean_val = mean, var_val = var),na.rm = T)

### tibble() : create your own tibble#######
# we'll use those as mock data for practice
T1 <- tibble(Site.ID = c("A", "B", "C", "D"), 
             Temperature = c(14.1, 16.7, 15.3, 12.8))

T2 <-tibble(Site.ID = c("A", "B", "D", "E"), 
            pH = c(7.3, 7.8, 8.1, 7.9))

### right_join() ######
left_join(T1, T2) # drops site E, absent in T1
right_join(T1, T2) # drops site C, absent in T2

### inner_join() vs full_join() ######
inner_join(T1, T2) # only keeps the rows in common, all columns
full_join(T1, T2) # keeps all the data and fills with NA

### semi_join() vs anti_join() ######
# semi_join keeps all rows from the first data set where 
# there are matching values in the second data set, 
# keeping just columns from the first data set.

# anti_join saves all rows in the first data set that do not match 
#anything in the second data set. 
#This can help you find possible missing data across datasets.

semi_join(T1, T2) # only keeps the column of T1
anti_join(T1, T2) # keeps unmatched data in T1

