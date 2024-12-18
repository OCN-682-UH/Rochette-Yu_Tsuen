Good Plot, Bad Plot
================
Keanu Rochette
2024-10-28

- [Load the Libraries](#load-the-libraries)
- [Load the Data](#load-the-data)
- [Let’s make a **bad** plot](#lets-make-a-bad-plot)
  - [Creating a table summary to later plot
    it.](#creating-a-table-summary-to-later-plot-it)
  - [Making seasonal plots](#making-seasonal-plots)
    - [Fall plot](#fall-plot)
    - [Spring Plot](#spring-plot)
  - [Adding images to the plots](#adding-images-to-the-plots)
  - [Converting the images in
    rasters](#converting-the-images-in-rasters)
  - [Merging all the elements to make the final
    plot](#merging-all-the-elements-to-make-the-final-plot)
  - [Why is this a *bad plot*?](#why-is-this-a-bad-plot)
- [Let’s make a **Good** Plot](#lets-make-a-good-plot)
  - [Ordering my factors](#ordering-my-factors)
  - [Plotting the nice plot](#plotting-the-nice-plot)
  - [Why is it a *good plot*?](#why-is-it-a-good-plot)

# Load the Libraries

``` r
library(tidyverse)
library(here)
library(ghibli)
library(ggrepel)
library(RColorBrewer)
library(magick)
library(patchwork)
library(graphics)
library(grid)
library(gganimate)
```

# Load the Data

``` r
chem <- read_csv(here("Badplot_Goodplot", "data", "chemicaldata_maunalua.csv"))
```

# Let’s make a **bad** plot

## Creating a table summary to later plot it.

For this data table, I calculated the overall yearly average of all
biogeochemical parameter.  
Then, I clustered the data by parameters and calculated the average of
each parameter by season and by zone.  
Then I calculated the difference of the season average of each
parameters compared to the overall average.  
The idea was to calculate something that resembles calculating
**anomalies** in climate studies.

``` r
chem_summary <- chem %>% 
  #pivoting longer so the biogeochem measurements are in one column
  pivot_longer(cols = c(Temp_in:percent_sgd), 
               names_to = "Parameters", 
               values_to = "Values")%>% 
  # the channel zone had to many NAs so I removed it
  filter(Zone != "Channel") %>% 
group_by(Parameters) %>% 
  # calculate the yearly average of each parameters
  mutate(average = mean(Values, na.rm = T)) %>% ungroup() %>%
  group_by(Season, Parameters, Zone) %>% 
  #now calculating the average by parameter, season and sample zones
  summarize(dist_mean = mean(Values, na.rm = T)-average) %>% 
  # there were many repeats at the end 
  distinct() %>% 
  drop_na() 
```

## Making seasonal plots

My original idea was to make a seasonal plot using facet_wrap().  
However, I also went to add an image representative of the seasons (fall
and spring). Unfortunately, I couldn’t figure out how to do it with a
faceted graph, so I decided to do separate graphs, add the images
individually and then, merge them with *Patchwork*.

### Fall plot

``` r
fall_bplot <- chem_summary %>% 
  filter(Season == "FALL") %>% # only keeping the fall data
ggplot(aes(x= Zone, y = dist_mean, label= Parameters)) + 
  # creating the bar chart
  geom_bar(aes(fill = factor(Parameters)),
           stat = "identity",
           position = position_dodge(width = 0.9),
           color = "grey", face = "bold") +
  # Adding labels but avoiding overlaps in the labels
  geom_text_repel(size = 4, position= position_jitter(width=0.5, height=60),
                  color = "#4CAF50") +
  # Creating the facet wrap appearance for future merging
  facet_wrap(~Season, ncol = 2) +
  # Aesthetic elements of the plot
  labs(title = "Seasonal Average Diferensse of Biogeochemicol ParameTers to Yearly Average in an SGD Zone in the state of Hawaii",
       subtitle = "Maunalua Bay",
       x = "Zone Sampled",
       y = "Parameter Avg Distansse to Yearly Average", 
       fill = "P@raMeTers") + 
  theme_grey() + 
  theme(plot.title = element_text(size=14, face = "bold", color = "#FFEB3B"),
        plot.subtitle = element_text(size=12, color = "#FF991C"),
        axis.title = element_text(size = 12, color = "#B2DFDB"),
        axis.text = element_text(size = 12, color = "#FDD835"),
        strip.text.x = element_text(size = 12, face = "bold", color = "#FFF9C4"),
        panel.background = element_rect(fill = "#FF9D00"),
        legend.position="none") +
  #choosing a color palette that is not optimal 
  scale_fill_brewer(palette= "RdYlGn")

fall_bplot
```

![](../outputs/fall_plot-1.png)<!-- -->

### Spring Plot

This plot is essentially the same as the previous one by for spring data

``` r
spring_bplot <- chem_summary %>% 
  filter(Season == "SPRING") %>% 
ggplot(aes(x= Zone, y = dist_mean, label= Parameters)) + 
  geom_bar(aes(fill = factor(Parameters)),
           stat = "identity",
           position = position_dodge(width = 0.9),
           color = "grey") +
  geom_text_repel(size = 4, position= position_jitter(width=0.5,height=50),
                  color = "#FF5722", face = "bold") +
  facet_wrap(~Season, ncol = 2) +
  labs(title = " ",
       subtitle = " ",
       x = "Zone Sapled",
       y = "Parameter Average Distance to Yearly Average",
       fill = "P@rameTters") + 
  theme_grey() + 
  theme(axis.title = element_text(size = 12, color = "#C5CAE9"),
        axis.text = element_text(size = 12, color = "#FDD835"),
        strip.text.x = element_text(size = 12, color = "#FFF9C4",
                                    face = "bold"),
        panel.background = element_rect(fill = "#45FF17"),
        legend.background = element_rect(fill = "#F7FF00"),
        legend.title = element_text(color="#A5D6A7"),
        legend.text=element_text(color = "#AED581")) +
  scale_fill_brewer(palette= "RdYlGn")

spring_bplot
```

![](../outputs/spring_plot-1.png)<!-- -->

## Adding images to the plots

``` r
leaf <- image_read("https://st2.depositphotos.com/1821481/5451/i/450/depositphotos_54517857-stock-photo-autumn-leaf.jpg")

daisy <- image_read("https://img.freepik.com/premium-photo/one-white-daisy-flower-isolated-white-background-flat-lay-top-view-floral-pattern-object_438009-5164.jpg?w=360")
```

## Converting the images in rasters

Not entirely sure what a raster is but, my understanding is that the
images where converted into their pixels so that they can be mapped
later on a grid.

``` r
gleaf <- rasterGrob(leaf, width = unit(1, "npc"), height = unit(1, "npc"))
gdaisy <- rasterGrob(daisy, width = unit(1, "npc"), height = unit(1, "npc"))
```

## Merging all the elements to make the final plot

We are using Magick and Patchwork here. - Magick is used to add the
raster images to the individual plots. - Patchwork allows to merge the
fall and spring plots in one figure with their images on it.

``` r
(fall_bplot +
  annotation_custom(gleaf, xmin = 1, xmax = 2, ymin = 250, ymax = 350)) +
  (spring_bplot +
  annotation_custom(gdaisy, xmin = 1, xmax = 2, ymin = 150, ymax = 250))
```

![](../outputs/bad_plot-1.png)<!-- -->

## Why is this a *bad plot*?

- **Labels**: labels on the graphs are unnecessary, overwhelming and
  make it difficult to read the graph.  
- **Color Palette** of the bar chart: the color ramp from green to red
  is not ideal for people with have colorblindness as those colors do
  not provide enough contrast to distinguish them clear. This makes
  ready the plot more difficult.  
- **Background colors**: the background colors were supposed to echo the
  season that is represented on the graph. However, the orange (fall)
  and green (spring) are the similar colors to those used for the color
  ramp. So, it makes reading the graph very difficult and it’s even more
  difficult for the color blind demographic.
- **Text colors**: The text color is not uniform, too many colors are
  represented on the plot making it overwhelming to the viewer. The
  colors of the text are close to the background color and not
  colorblind friendly, making the plot hard to read.  
  The axis text, titles and strip text are light in color and don’t
  contrast enough with the white background.  
  The legend was plotted in a bright yellow color and just echoes the
  loudness of the plots. The text of the legend is very light which is
  hard to read on a yellow background.  
  Overall, the color on not ideal as they overwhelm the viewer and
  require additional focus to try understanding the plot.
- **Images** : the images in the corner are meant to echo and visually
  represent the seasons plotted on the graph. However, it was
  complicated to get to that result… So a waste of time. They are
  redundant as 2 other elements (facet titles and background color) are
  meant to show the seasons on this plot. They only crowd the plot more,
  overwhelming the viewer which makes it more difficult to easily read
  the information presented on the graph.
- **Grid**: this theme shows a white grid that contrast with the
  background color and make the plot appear overall more busy.  
- **Axes**: the axes are the not the same and it’s hard to compare the
  data by season.
- **Spelling**: There are typos throughout the plot.
- **Analytic method**: not the best to study changes by seasons.
- ***Redeaming quality***: It’s not a 3D plot! Only because I couldn’t
  figure out how to do it…

Like the Gen Zs say:

> “It’s not giving…”

# Let’s make a **Good** Plot

## Ordering my factors

I want to know the order of the Zones, i.e., which one is closed to the
shore and which one is fardest.

``` r
chem %>% select(Zone, Lat, Long) %>% 
  drop_na() %>% 
  group_by(Zone) %>% 
  summarise(Lat = min(Lat), Long = min(Long)) %>% 
  arrange(Long)
```

    ## # A tibble: 6 × 3
    ##   Zone          Lat  Long
    ##   <chr>       <dbl> <dbl>
    ## 1 Near Spring  21.3 -158.
    ## 2 Transition   21.3 -158.
    ## 3 Diffuse      21.3 -158.
    ## 4 Ambient      21.3 -158.
    ## 5 Offshore     21.3 -158.
    ## 6 Channel      21.3 -158.

## Plotting the nice plot

``` r
chem %>% 
  #pivoting longer so the biogeochem measurements are in one column
  pivot_longer(cols = c(Temp_in:percent_sgd), 
               names_to = "Parameters", 
               values_to = "Values")%>% 
  # channel and near spring zone had too many NAs so I removed them for visuals
  filter(Zone != "Channel", Zone != "Near Spring") %>%
  # I'm making a violin plot, parameters that are highly variables will not render nicely so I am only keeping parameters vary less.
  filter(Parameters == "TA"| Parameters == "Phosphate"|Parameters == "pH" |
         Parameters == "Temp_in") %>%
  #Ordering the zones from shore to ocean. 
  mutate(Zone = factor(Zone, levels= c("Transition", "Diffuse","Ambient", "Offshore"))) %>% 
  ggplot(aes(x= Zone, y= Values, fill = Zone)) +
  #making violin plots as my primary geometry
  geom_violin(width=1.15)+
  #adding box plots as secondary geometry inside the violin plots to add more information to the graph
  geom_boxplot(width=0.1, color="grey", alpha=0.2)+
  # splitting the data by parameters with free scales because the measurememts are not in the same unit.
  # Also relabeling the strip text
  facet_wrap(~ Parameters, scale= "free", nrow = 2,
       labeller = labeller(Parameters= c(
          "Phosphate" ="Phosphate (µmol/L)",
          "TA" = "Total Alkalinity (µmol/kg)",
          "Temp_in" = "Temperature (°C)"))) +
  #add nicer labels
  labs(title = "Water Quality Parameters in Maunalua Bay",
       subtitle = "Data Collected from the Silbiger Lab",
       x = "Zones in the Fringing Reef",
       y = "Measured Values"
       )+
  theme_bw() +
  theme(plot.title = element_text(size=14, face = "bold"), 
        plot.subtitle = element_text(size=12),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=12, face = "bold"),
        legend.text=element_text(size=12),
        axis.text.x = element_text(),
        panel.background = element_rect(fill = "azure1"),
        legend.position = "none")+
  #custom colors
  scale_fill_ghibli_d("SpiritedMedium", direction=-1)
```

![](../outputs/good_plot-1.png)<!-- -->

## Why is it a *good plot*?

- **Title and Subtitle**: They are short and simple, providing only the
  necessary information to the viewer. The bolded title stands out from
  the subtitle.
- **Axis title and text**: They are sized correctly, clean and only
  convey necessary information. The axis are not the same across the
  plots, however it make sense since they are not displaying the same
  information.
- **Facet wrap**: the plot is facets by selected measurements. Only the
  measurements that made sense were selected to be plotted. They are
  bolded for clarity and show the units in which the measurements can be
  read so there is no confusing.
- **Analytical method**: the primary geometry is a violin plot which is
  meant to show the density distribution of the data. We added a box
  plot in the violin plots such that box plots are fairly discreet. They
  provide additional information such as the quantiles and the median of
  the data.
- **Color palette**: the colors selected come from the ghibli package.
  They are high contrast with each other and the background so it’s easy
  to read the plot. The cooler pastel tones make are also typically more
  relaxing to look at than loud bright neon colors.
- **Background color**: The very light blue background adds a nice touch
  of color to the plot without being obnoxiously loud and crowding the
  plot.
- **Zone Order**: The sampling zones where ordered from shore to ocean
  to make the data easily readable. Patterns and differences from shore
  to ocean will also be more noticeable that way.
- **Figure Legend**: The legend was omitted because it was redundant and
  not useful here.
- **Spelling**: There were efforts to remove any typos from the final
  plots.
