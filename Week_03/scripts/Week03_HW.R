## Week 3 HW: create your own plot using the Penguin data 

## Created : 2024-09-12
## Created by: Keanu Rochette-Yu Tsuen 
##############

### Libraries ########
library(tidyverse) #data wrangling package
library(palmerpenguins) #contains dataset
library(here) # unbreakable pathway
library(ghibli) #color palettes 
library(ggthemes) #for ggplot themes

### Read the data ########
glimpse(penguins) #short hand to see the data
view(penguins)

### Data visualization ##########
#Density plot of penguin bill length by sex, separated by species and island

ggplot(penguins, aes(x=bill_length_mm, group = sex, fill =sex))+ 
                  #load data to be plotted with specific variables, group the data by sex
  geom_density(alpha = 0.7)+ #change the transparency of the fill of the density plot
  facet_grid(island~species) + # split the data by species and by islands
  labs(title = "Density plot of bill length between sexes",
       subtitle = "Data separated by species and by island",
       x = "Bill Length (mm)",
       y = "Density", #give pretty names to the plot and axes
      fill = "Sex") + 
  theme_bw() + 
  scale_fill_ghibli_d("SpiritedMedium", direction =-1) + 
              # color of the fill of the curves set to a ghibli themed pallette
  theme(plot.title = element_text(size=20,color = "darksalmon"), 
                      #edit the title of the plot
        plot.subtitle = element_text(size=14,color = "darksalmon"),
                      #edit the subtitle of the plot
        axis.title = element_text(size = 16, color = "deepskyblue1"),
                      #edit the axis title of the plot
        axis.text = element_text(size = 20, color= "aquamarine"),
                      #edit the axis labels tickmarks of the plot
        panel.background = element_rect(fill = "azure1"), 
        strip.text = element_text(size = 16))
                      # changes the headers of the facet grid


#save the plot object
ggsave(here("Week_03", "outputs", "density_plot_penguins_HW.png"), 
       width=10, height = 6)


