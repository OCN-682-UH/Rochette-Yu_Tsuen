### Practicing with join and lubridate ######
### Created by: Keanu Rochette-Yu Tsuen
### Date: 2024-09-25

### Libraries ##########
library(tidyverse)
library(here)
library(lubridate)
library(ghibli)

### Load the Data #########
cond_data <- read_csv(here("Week_05", "data", "CondData.csv"))
depth_data <- read_csv(here("Week_05", "data", "DepthData.csv"))

# converting date to datetime 
cond_data <- cond_data %>% mutate(date = mdy_hms(date)) %>% 
  mutate(date = round_date(date, "10 seconds"))

# joining the 2 dataframes
joined_df <- inner_join(cond_data, depth_data)

# Calculating the average of each variable by minute and graphing it
joined_df %>% mutate(date = round_date(date, "minute")) %>% 
              #round the data to the nearest minute 
  rename(Absolute_Pressure = AbsPressure) %>% 
              # I didn't like the name so I made it more explicit 
  pivot_longer(cols = c(2:5, -3), names_to = "Parameters", 
               values_to = "Values") %>% 
              # Turn the data in long format for summary 
  group_by(date, Parameters) %>% # group by minutes and parameters
  summarize(mean_val = mean(Values), .groups = NULL) %>% 
              # calculating the average and removing grouping 
  ggplot(aes(x= date, y = mean_val, color = Parameters)) +
              # setting the mapping of the graph
  geom_point() + # scatter plot
  geom_line() + # with a line that connects the dots
  facet_wrap(~ Parameters, ncol = 1, scales = "free") +
              # Display the graphs in one column by parameter
              # Each of the graph has its only y-axis units
  labs(title = "Average Physico-Chemical Values by Minute",
       subtitle = "Taken in Moorea, FP",
       x = "Date-Time",
       y = "Average Values (units are different)")+ #give explicit labels
  theme_bw() +
  theme(plot.title = element_text(size=20,color = "#FD8A8A"), 
        #edit the title of the plot
        plot.subtitle = element_text(size=14,color = "#9EA1D4"),
        #edit the subtitle of the plot
        axis.title = element_text(size = 16, color = "#A8D1D1"),
        #edit the axis title of the plot
        axis.text = element_text(size = 12, color= "#A7BED3"),
        #edit the axis labels tickmarks of the plot
        panel.background = element_rect(fill = "azure"),
        legend.position = "none") +
        # removing the legend, unecessary here in my opinion
  scale_color_ghibli_d("SpiritedMedium", direction = -1)
  # decide on cool ghibli inspired colors for the graph

#save the plot in output folder
ggsave(here("Week_05", "outputs", "Week_05_HW.png"), 
       width=10, height = 6)
 




