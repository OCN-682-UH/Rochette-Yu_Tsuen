# Homework Week 4:

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
library(ghibli)

### Load Data #######
glimpse(penguins)

##########
# 1) calculates the mean and variance of body mass by species, 
# island, and sex without any NAs.
penguins %>% #load the dataset
  drop_na(species, island, sex) %>% # remove NA. na.omit() also an option
  group_by(species, island, sex) %>% # group the data by spp., island and sex
  summarise(mean_mass_g = mean(body_mass_g),  
            variance = var(body_mass_g)) 
#create a summary table with mean body mass and variance by groups defined above

###########
# 2) Filters out male penguins, calculates the log body mass.
# Select only the columns for species, island, sex, and log body mass.
# Use these data to make any plot.
penguins %>% #load the dataset 
  filter(sex !="male") %>% # remove the male peguin data
  mutate(log_mass = log(body_mass_g)) %>% 
            # creates a new column while also calculating the log of the mass
  select(species, island, sex, log_mass) %>% 
            # only selects the species, island, sex and log mass columns
  ggplot(aes(x= species, y=island, fill=log_mass)) +
            # select the variables that will be represented in ggplot graph
  geom_tile() + #creates a heatmap
  labs(title = "Halloween Themed Heatmap of log mass in female Palmer penguins",
       subtitle = "Data separated by species and by island",
       x = "Species",
       y = "Island", #give pretty names to the plot and axes
       fill = "Log Scale Mass") + 
  theme_bw() + 
  scale_fill_ghibli_c("LaputaMedium", direction =-1) + 
  # color of the heatmapis a ghibli themed palette
  theme(plot.title = element_text(size=20,color = "chartreuse4"), 
        #change the color of the title of the plot
        plot.subtitle = element_text(size=14,color = "chartreuse3"),
        #change the color the subtitle of the plot
        axis.title = element_text(size = 16, color = "#FB702D"),
        #change the color the axis title of the plot
        axis.text = element_text(size = 20, color= "#FFC341"),
        #change the color the axis labels tickmarks of the plot
        panel.background = element_rect(fill = "#fbfaf4"))
        #change the color of the background

#save the plot object
ggsave(here("Week_04", "outputs", "Halloween_Heatmap_HW.png"), 
       width=10, height = 6)





