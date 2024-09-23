### Learning about tidyr using biogeochem data in Hawaii
### Created: KRYT
### Date: 2024-09-17

#### Libraries ##########
library(tidyverse)
library(here)

### Load data #######
chemdata <- read.csv(here("Week_04", "data", "chemicaldata_maunalua.csv"))
glimpse(chemdata)

chemdata_clean <- chemdata %>% 
  filter(complete.cases(.)) #filter everything that's not a complete row

### separate() #############
#### Fixing tide_time column data to have 2 separate columns 
chemdata_clean <- chemdata %>% 
  drop_na() %>% #removes NA containing rows
  separate(col = Tide_time, #select the column to separate
           into = c("Tide","Time"), #what are the new column names to send data
           sep = "_")# separated by "_"
#tide_time gets delete in the process. 
#if not, use separate(...., remove = F)

### unite() ############# very similar to separate()
# bring 2 columns together. 
chemdata_clean <- chemdata %>% 
  drop_na() %>% #removes NA containing rows
  separate(col = Tide_time, #select the column to separate
           into = c("Tide","Time"), #what are the new column names to send data
           sep = "_", 
           remove= F) %>% 
  unite(col = "Site_Zone", #what column to create, need " " marks
        c(Site,Zone), #which columns to unite, no need quotes, exists already
        sep = ".", # how to separate the 2 values in the new column
        remove = F) #keep the original columns


### Wide vs Long data #############
# Wide data: 1 observation per row and all different variables are columns
# Long data: 1 unique measurement per row 
# and all the info about measurement in the same row 

#### pivot_longer() ##########
# create a column with all the biogeochem parameters+measurements 
# but metadata stays the same

chemdata_long <- chemdata_clean %>% 
  pivot_longer(cols = Temp_in:percent_sgd, # columns to pivot, from temp to %sgd
               names_to = "Variables", 
                        # name of the new column with the biogeochem parameters
               values_to = "Values") 
                  #new column with the values of the biogeochem parameters

#### what to do with long data ##########
chemdata_long %>% 
  group_by(Variables, Site) %>% # group by specific parameters
  summarise(Param_mean = mean(Values, na.rm =T), #get the mean 
            Param_vars = var(Values, na.rm = T)) # get the variance
# exercise
chemdata_long %>% 
  group_by(Variables, Site, Tide) %>% 
  summarise(Param_mean = mean(Values, na.rm =T), 
            Param_vars = var(Values, na.rm = T), 
            Param_sd = sd(Values, na.rm=T)) # get the variance

#### facet_wrap() on long data ##########

chemdata_long %>% 
  ggplot(aes(x = Site, y = Values)) +
  geom_boxplot()+
  facet_wrap(~Variables, scales = "free")
      #allows the scales of the x&y to be independent from one another
      # facet_wrap(scales = "free_x") only the x is "free"

#### pivot_wider() ##########
chemdata_wide <- chemdata_long %>% 
  pivot_wider(names_from = Variables, # column with the name of the new columns
              values_from = Values) # column with the values of the variables

#### summary statistics to csv ##########
chemdata_clean <- chemdata %>% 
  drop_na() %>% 
  separate(col = Tide_time, 
           into = c("Tide","Time"), 
           sep = "_", 
           remove= F) %>% 
  pivot_longer(cols = Temp_in:percent_sgd, 
               names_to = "Variables", 
               values_to = "Values") %>% 
  group_by(Variables, Site, Time) %>% 
  summarize(mean_vals=mean(Values, na.rm = T)) %>% 
  pivot_wider(names_from = Variables,
              values_from= mean_vals) %>% 
  write_csv(here("Week_04", "outputs", "summary.csv"))
  

  
