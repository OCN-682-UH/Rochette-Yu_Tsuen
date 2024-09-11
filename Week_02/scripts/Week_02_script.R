## This is my first script. 
## Week 2 is about learning proper data structure and 
## unbreakable file paths.

## Created : 2024-09-04
######################################################

### Load libraries ###########
library(tidyverse)
library(here)

### Read the data ####
weightdata <- read.csv(here("Week_02", "data", "weightdata.csv"))


###Data Analysis #####
head(weightdata) # shows the top 6 lines
tail(weightdata) # shows the last 6 lines
view(weightdata) # shows all the data in a new tab

