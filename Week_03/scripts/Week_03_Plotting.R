## Intro to graphing with ggplot

## Created : 2024-09-10
## Created by: Keanu Rochette-Yu Tsuen 
##Updated : 2024-09-11
##############

### Libraries ########
library(tidyverse)
library(palmerpenguins)
library(here)
library(beyonce) #color palettes 
library(ggthemes)

### Read the data ########
view(penguins) #data file is penguins
glimpse(penguins) #short hand to see the data, more concise than str()
str(penguins)


### Data visualization ##########

#### Playing with mapping : aes() ##############
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
                      y = bill_length_mm)) + 
    geom_point(size = 5) #Applied to all data point  


#### Playing with facetting ###########
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

#### Adding more stuff to the graph ##########
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      color = species)) + 
  geom_point() +
  facet_grid(species ~ sex) +
  scale_color_viridis_d() +
  guides(color = F) # removes the legend

#### Regressions in graphs ##########
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm)) + 
  geom_point() +
  geom_smooth(method = "lm", # best fit line to data, ex: regression line ("lm")
             se = F) + # "se" is standard error, removed
  labs(x = "Bill Depth (mm)",
       y = "Bill Length (mm)")

#we can make a regression by groups
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      group =species, #group by species for the regressions
                      color = species )) + # color by spp
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Bill Depth (mm)",
       y = "Bill Length (mm)") +
  scale_color_viridis_d() # add a color scale

#### Changing the scale of color on plots #####
# > (1) scale 
# > (2) name of the primary aes (color, shape, x....)
# > (3) name of the scale (continuous, discrete, manual)
# ex: scale_color_continous(), scale_x_continous()

# ex: changing the bounds on axes
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      group =species, 
                      color = species )) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Bill Depth (mm)",
       y = "Bill Length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(limit=c(0,20)) + # changes bounds of the x-axis
  scale_y_continuous(limit=c(0,50)) # changes bounds of the y-axis
#don't forget c() "concatonate" to make a list of numbers defining the bounds

#adding specific  breaks to the axis 
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      group =species, 
                      color = species )) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Bill Depth (mm)",
       y = "Bill Length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(14, 17, 20),#only 14, 17 and 20 tickmarks show
                     labels = c("low", "medium", "high")) # replace the numbers
                      # on tickmarks by string labels

# Choosing specific colors manually
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      group =species, 
                      color = species )) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Bill Depth (mm)",
       y = "Bill Length (mm)") +
  scale_color_manual(values= c("orange", "purple", "green")) 
# choosing colors and having them in a specific order
  
# Using pre-made color palette
# ex: devtools::install_github("dill/beyonce") 
# need to use the github installation for packages not approved by CRAN
# go to the github and read the README to see how to use 

p <- ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      group =species, 
                      color = species )) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Bill Depth (mm)",
       y = "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(2)) 
 #in the manual color scale, call the beyonce_palette() function 
## specify the palette number that you want to use


### Coordinates in graphs
# coord_flip()
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      group =species, 
                      color = species )) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Bill Depth (mm)",
       y = "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(2)) +
  coord_flip() #flip x and y


#coord_fixed(), specify the aspect ratio
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      group =species, 
                      color = species )) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Bill Depth (mm)",
       y = "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(2)) +
  coord_fixed(ratio = 1.5) # specify the aspect ratio


# coord_trans(), transform the axis (log, sqrt)
library(diamonds)

ggplot(diamonds, 
       aes( x = carat, y = price)) + 
  geom_point() +
  coord_trans (x = "log10", y = "log10") 
#allows to keep the raw data plotted without having to alter it with calculations
# but to change it visually
# can also do log, sqrt...etc.

#coord_polar(), windrose plots
ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      group =species, 
                      color = species )) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Bill Depth (mm)",
       y = "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(2))+
  coord_polar("x") # change to polar coordinate in the x- axis

### Themes ###########

#### Default themes ########
p + theme_bw() # change the theme to black and white
p + theme_grey()
p + theme_classic()

#### Custom Themes ##########
p + theme_wsj() # wallstreet journal style
# more documentation on the ggtheme website

#### Changing anything in the theme (theme()) ########
p + theme_bw() +
  theme(axis.title = element_text(size = 20, #changes applied to x and y, 
                                              #can be specified 
                                  color = "red"),#change color to red
        panel.background = element_rect(fill = "linen", #changes the background
                                        color = "red"),#changes the outline
        axis.ticks.x = element_line(linewidth = 10))  # change the width of 
                                                        #the tick marks


### Save your plot ###########
#save it as an image
ggsave(here("Week_03", "outputs", "penguin.png"), 
            width=7, height = 5) #save at a specific size, otherwise, 
                                  #it gets squished

#save it as an element in the environment
plot1<- ggplot(data = penguins, 
       mapping = aes( x = bill_depth_mm, 
                      y = bill_length_mm,
                      group =species, 
                      color = species )) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Bill Depth (mm)",
       y = "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(2)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20, #changes applied to x and y, 
                                  #can be specified 
                                  color = "red"),#change color to red
        panel.background = element_rect(fill = "linen", #changes the background
                                        color = "red"),#changes the outline
        axis.ticks.x = element_line(linewidth = 10))

#save the plot object

ggsave(here("Week_03", "outputs", "plot1.png"), 
       width=7, height = 5)
