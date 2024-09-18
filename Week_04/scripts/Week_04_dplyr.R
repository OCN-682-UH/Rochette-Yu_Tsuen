### Learning about dplyr
### Created: KRYT
### Date: 2024-09-17

#### Libraries ##########
library(tidyverse)
library(palmerpenguins)
library(here)

### Load data #######
glimpse(penguins)
view(penguins)

#### filter() ########
#### logical operator : "==" exactly equal to
filter(penguins, sex == "female") #only selects the females

#### Exercise 
filter(penguins, year == "2008") # only 2008 data
filter(penguins, body_mass_g > 5000) # body mass greater than 5000g

#### filter with multiple conditions
filter(penguins, year == "2008", body_mass_g > 5000) #this AND that are true
filter(penguins, year == "2008" & body_mass_g > 5000) # same as the "," 
filter(penguins, year == "2008" | body_mass_g > 5000) # this OR that


#### exercise
filter(penguins, year == "2008" | year == "2009") #year 2008 OR 2009
filter(penguins, year %in% c("2008","2009")) #year within 2008-2009
filter(penguins, island != "Dream" ) # NOT island dream
filter(penguins, species %in% c("Gentoo", "Adelie")) # !!!!!!!!!!!
#if you select 2 values in the same variable you need to use "%in%"

#### mutate() ######## 
#### Add a new column with new values/calculations
data2 <- mutate(penguins, body_mass_kg = body_mass_g/1000)

# can also add more columns at once
data2 <- mutate(penguins, 
                body_mass_kg = body_mass_g/1000,
                bill_length_depth= bill_length_mm/bill_depth_mm)
# !!!!!!!!! ALSO check mutate_if(), mutate_at(), mutate_all()


#### ifelse() ######## 
data2 <- mutate(penguins, after_2008 = ifelse(year>2008, 
                                              "After 2008", 
                                              "Before 2008"))
#in the column "after_2008", if the date is >2008, 
# write "After 2008", else, fill in with "Before 2008". 
# Only 2 outputs possible
# !!!!!!!!! check also casewhen(), you can add many more stuff as output options


#exercise
data2 <- mutate(penguins, 
                sum_flipper_mass = flipper_length_mm + body_mass_g,
                 penguins_size = ifelse (body_mass_g>4000, "big", "small"))

#### %>% "and then do" aka the "pipe" ######## 
penguins %>% 
  filter(sex = "female") %>% 
  mutate(XXXXXXXX) 

#### select() ######## 
#only selects columns of interests and renames some columns
penguins %>% # use penguin dataframe
  filter(sex == "female") %>% #select females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log biomass
  select(species, island, sex, log_mass)

#### summarize() ######## 
# generates a table of summarized data don't forget to use "na.rm = true"
penguins %>% # 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE),
            min_flipper = min(flipper_length_mm, na.rm=TRUE))

#### group_by() ######## 
# cluster by categories
penguins %>%
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))

#### drop_na() ######## 
#removes the NA in a specified column
penguins %>%
  drop_na(sex) %>%
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

#### integrate into ggplot ######## 
# no need to save the dataframe if piped into ggplot
# remember that ggplot has a specific syntax "+" not " %>% "
penguins %>%
  drop_na(sex) %>%
  ggplot(aes(x = sex, y = flipper_length_mm)) +
  geom_boxplot()



