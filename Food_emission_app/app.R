# Title : Food Emissions Shiny App
# Created by: Keanu Rochette
# Date Creation: 2024-11-16
############################

# Load Libraries
library(shiny)
library(tidyverse)
library(here)
library(patchwork)
library(shinythemes)
library(gt)

# Load Data
tuesdata <- tidytuesdayR::tt_load('2020-02-18')
food <- tuesdata$food_consumption

########################## User Interface ################################## 
# User Interface
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  titlePanel("An Example on Using RShiny - Carbon Emissions in Food"),
  
  h4("Created by: Keanu Rochette"),
  h4("Date: 2024-11-16"),
  
  h3("🌎 Food Consumption and Carbon Emission by Country 🌎"),
   
  fluidRow(
    column(5,
  # Selection Box for the 1 bar chart 
  selectInput(inputId = "country", 
              label = h4("Select a Country"), # h3 is the font size of the label
              # sort by alphabetical order
              # creates drop down list with country options using unique()
              choices = unique(sort(food$country)), 
              selected = "French Polynesia")),
  column(5,
         p("We can use this interactive drop-down menu on the left
             to look at different countries' consumption of food categories 
             and the carbon emissions associated with those food."),
         p("Let see what each country eats the most and what food groups 
           contribute the most to their carbon emissions."))),
  
  plotOutput("food_conso"),
  
  h3("🍔 Country Consumption and Emission by Food Category 🍕"),
  
  fluidRow(
    column(6,
           selectInput(inputId = "food_cat", 
              label = h4("Select a Food category"), 
              # sort by alphabetical order
              # creates drop down list with country options using unique()
              choices = unique(sort(food$food_category)), 
              selected = "")),
    column(6,
           sliderInput(inputId = "quantile", 
                       label = h4("Choose a Quantile"), 
                       value = 85, min = 75, max = 100
           ))),
  
  plotOutput("country_conso")
  
)

######################## Back End Info #####################################  
# Back End Info of the app
server <- function(input,output){
  
  data1 <- reactive({
    
    food %>% arrange(country) %>% 
      group_by(country) %>% 
      mutate(conso_tot = sum(consumption, na.rm = T),
             emission_tot = sum(co2_emmission, na.rm = T)) %>% ungroup() %>% 
      filter(country == input$country) %>% 
      group_by(food_category)
    
  })
    
    data2 <- reactive({
    food %>% 
      group_by(food_category) %>% 
      filter(food_category == input$food_cat,
             consumption >= quantile(consumption, input$quantile/100, na.rm = T))  
    
  })
  
  # Graphing Plot 1
  output$food_conso <- renderPlot({
    
    conso_graph <- data1() %>% 
      summarize(conso_proportion = consumption/conso_tot) %>% 
      mutate(food_category = fct_reorder(food_category,conso_proportion)) %>% 
      ggplot(aes(x = food_category , y= conso_proportion)) +
      geom_bar(stat= "identity", fill = "#43A047")+
      geom_text(aes(label= paste(round(conso_proportion*100), "%")),
                hjust = -0.5, fontface = "bold")+
      coord_flip()+ 
      labs(title = "Food consumption in:",
           subtitle = paste0(input$country),
           x = "Food Categories",
           y= "Consumption Proportion (%)")+
      scale_y_continuous(labels = scales::percent, limits=c(0,1))+
      theme_bw() + 
      theme(plot.title = element_text(size=16, face = "bold"), 
            plot.subtitle = element_text(size=15), 
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 14),
            legend.position = "none",
            panel.background = element_rect(fill = "azure1"))
    
    emission_graph <- data1() %>% 
      summarize(emission_proportion = co2_emmission/emission_tot) %>% 
      mutate(food_category = fct_reorder(food_category,emission_proportion)) %>% 
      ggplot(aes(x = food_category , y= emission_proportion)) +
      geom_bar(stat= "identity", fill = "#455A64")+
      geom_text(aes(label= paste(round(emission_proportion*100), "%")),
                hjust = -0.5, fontface = "bold")+
      coord_flip()+ 
      labs(title = "CO2 Emissions Fraction in:",
           subtitle = paste0(input$country),
           x = "Food Categories",
           y= " Proportion of CO2 Emission (%)")+
      scale_y_continuous(labels = scales::percent, limits=c(0,1))+
      theme_bw() + 
      theme(plot.title = element_text(size=16, face = "bold"), 
            plot.subtitle = element_text(size=15), 
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 14),
            legend.position = "none",
            panel.background = element_rect(fill = "azure1"))
    
    conso_graph + emission_graph + plot_layout(axes = "collect")
    
  })
  
  # Graphing Plot 2
  output$country_conso <- renderPlot({
    
    country_conso <- data2() %>% 
      mutate(country = fct_reorder(country,consumption)) %>% 
      ggplot(aes(x = country , y= consumption)) +
      geom_bar(stat= "identity", fill = "#1565C0")+
      geom_text(aes(label= round(consumption)),
                hjust = -0.5, fontface = "bold")+
      coord_flip()+ 
      labs(title = paste("Total Consumption of",input$food_cat),
           subtitle = paste("Top", 100-input$quantile,
                            "% of the Consuming Countries"),
           x = "Countries",
           y= "Total Consumption (kg/pers/yr)")+
      theme_bw() + 
      theme(plot.title = element_text(size=16, face = "bold"), 
            plot.subtitle = element_text(size=15), 
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 14),
            legend.position = "none",
            panel.background = element_rect(fill = "azure1"))
    
    country_emission <- data2() %>% 
      mutate(country = fct_reorder(country,co2_emmission)) %>% 
      ggplot(aes(x = country , y= co2_emmission)) +
      geom_bar(stat= "identity", fill = "#455A64")+
      geom_text(aes(label= round(co2_emmission)),
                hjust = -0.1, fontface = "bold")+
      coord_flip()+ 
      labs(title = paste("CO2 Emissions from",input$food_cat),
           subtitle = paste("Top", 100-input$quantile,
                            "% of the Consuming Countries"),
           x = "Countries",
           y= "Total Emissions (kgCO2/pers/yr)")+
      theme_bw() + 
      theme(plot.title = element_text(size=16, face = "bold"), 
            plot.subtitle = element_text(size=15), 
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 14),
            legend.position = "none",
            panel.background = element_rect(fill = "azure1"))
    
    country_conso + country_emission + plot_layout(axes = "collect")
    
  })
  
  
}



####################################################################### 
# Rendering the app
shinyApp(ui = ui, server = server)







