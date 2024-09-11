## Intro to graphing with ggplot

## Created : 2024-09-10
##############

### Libraries ########
library(tidyverse)
library(palmerpenguins)

### Read the data ########
view(penguins) #data file is penguins
glimpse(penguins) #short hand to see the data, more concise than str()
str(penguins)


### Data visualization ##########

#### Playing with mapping : aes()
ggplot(data = penguins, #load the data in the function
       mapping = aes( x = bill_depth_mm, # add the x and y data that will be plotted
            y = bill_length_mm,
            color = species, # add species specific color
            shape = species,  # create a shape for category of variable
            size = body_mass_g, # varies the size based on data
            alpha = flipper_length_mm)) +#varies transparency based on data
  geom_point() + # how to represent the data on a graph
  labs(title= "Bill depth and length", # add title
      subtitle = "Dimensions of bill by species", # add subtitle
      x = "Bill Depth (mm)", # add pretty label x and y axes 
      y = "Bill Length (mm)", 
      color = "Species", # change the legend title
      caption = "Source : Palmer Station LTER / palmerpenguins package") +
        #add caption, good for citation !
  scale_color_viridis_d() # pre-made color scale, d = "discreet"

#### Playing with settings: independent of data 
ggplot(data = penguins, #load the data in the function
       mapping = aes( x = bill_depth_mm, # add the x and y data that will be plotted
                      y = bill_length_mm)) + #varies transparency based on data
    geom_point(size = 5) #Applied to all data point  


#### Playing with facetting
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm)) + 
  geom_point() +
  facet_grid(sex ~ species) # separate the data by spp and sex in a grid
# facet grid provides default dimension aka a grid

ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm)) + 
  geom_point() +
  facet_wrap(~ species, ncol = 3) # like a ribbon that wraps on itself
# more flexibility than grid because can specify the dimensions

#### Adding more stuff to the graph
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      color = species)) + 
  geom_point() +
  facet_grid(species ~ sex) +
  scale_color_viridis_d() +
  guides(color = F) # removes the legend



