### Homework to practice using Tidyr
### Created: KRYT
### Date: 2024-09-19
### Edited : 2024-09-22

#### Libraries ##########
library(tidyverse)
library(here)
library(RColorBrewer)
library(ghibli)

### Load data #######
chemdata <- read.csv(here("Week_04", "data", "chemicaldata_maunalua.csv"))
glimpse(chemdata)

### Cleaning the data ########
chem_tidy <- chemdata %>% 
  drop_na() %>% # removes NA
  tibble() %>% #make the dataframe a tibble, easier to view in console
  separate(col = Tide_time, into = c("Tide", "Time"), sep = "_", remove = F)
        #create new column for the data separated from tide_time

### create a csv of summary statistics ######
chem_tidy %>% 
 group_by(Season, Time, Zone) %>% #create groups to cluster data points
  summarise(max_sal = max(Temp_in), #max value for temperature
            mean_sal = mean (Temp_in), #average  for temperature
            var_sal = var(Temp_in)) %>% #variance for temperature
  write_csv(here("Week_04", "outputs", "temp_summary_HWb.csv"))


### make an interesting graph #########
chem_tidy %>% 
  select(-Lat, -Long, -Tide_time,) %>% #only select relevant columns
  pivot_longer(cols = Temp_in:NN, # columns to pivot
               names_to = "Variables", 
               # name of the new column with the biogeochem parameters
               values_to = "Values") %>% # column of the data values
  group_by(Zone, Variables) %>% # group by zones and variables
  summarise(mean_val = mean (Values), sd_val = sd (Values), 
            .groups = 'drop') %>% 
            #summary statics (mean and sd), needed to drop groups to be able 
            # to graph the data, somehow...
  ggplot(aes(x = Zone, y = mean_val, group = Variables,
             color = Variables, shape = Variables)) + 
              # setting the variable to plot
  geom_point(size = 3) +
  geom_line(aes(group=1), linewidth=1) + 
      #there is only 1 values in each group, so had to set "group" to 1
      # Linewidth changes the thickness of the lines
  geom_errorbar(aes(ymin=mean_val-sd_val, ymax=mean_val+sd_val))+
      # putting error bars to the graph
  facet_wrap(~Variables, nrow = 1, scales = "free") +
      # plot graphs by varibales in 1 row with independant scales
  labs(title = "Biogeochemical Parameters in Lagoon",
       subtitle = "Gradient of variables ",
       x = "Zone sampled ",
       y = "Concentration (units are different)", #give pretty names to the plot and axes
       fill = "Variables",
       caption = "Note: Order the zones based on location to shore") + 
  theme_bw() + 
  theme(plot.title = element_text(size=20,color = "#6C2F00"), 
        #edit the title of the plot
        plot.subtitle = element_text(size=14,color = "#9E682A"),
        #edit the subtitle of the plot
        axis.title = element_text(size = 16, color = "#F1B930"),
        #edit the axis title of the plot
        axis.text = element_text(size = 12, color= "#8A9748"),
        #edit the axis labels tickmarks of the plot
        panel.background = element_rect(fill = "#FFFAF1"),
        legend.position = "none") +
        #removes the legend, redundant here
  scale_x_discrete(guide = guide_axis(n.dodge=2))
        # prevents the x-axis labels to overlap

#save the plot object
ggsave(here("Week_04", "outputs", "chemdata_HW.png"), 
       width=12, height = 6)



