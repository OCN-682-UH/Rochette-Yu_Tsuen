# Title : Final Project - Oceanographic Data
# Created by: Keanu Rochette
# Date Creation: 2024-11-29
############################

# Load Libraries
library(shiny)
library(tidyverse)
library(here)
library(shinythemes)


#Load the Data
fcm <- read_csv(here("data", "fcm.csv"))
CTD <- read_csv(here("data", "CTD.csv"))
CTD_FCM <- read_csv(here("data", "CTD_FCM.csv"))

# Define UI 
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  titlePanel("🌊 Oceanographic Data: a Story of the Ocean 🌊"),
  
  h5("Created by: Keanu Rochette-Yu Tsuen"),
  h5("Date: 2024-11-29"),
  
  h3("🚢 Sampling Map to the West of Oahu 🚢"),
  
  # Add an image to the shiny app
  img(src ="cast_oahu.png", height="50%", width="50%", align = "center"),
  
  h5("In September 2023, a cruise onboard the R/V Kilo Moana set sails to the 
     West of Oahu to investigate the magnitude of the island Mass Effect."),
  
  # First row displays the CTD data, physical parameters 
  h3("🧪 Physical Parameter Profile 🧪"),
  
  fluidRow(
    column(3,
  
  selectInput(inputId = "station", 
              label = h5("Select a Station"), 
              choices = unique(sort(CTD$station)), 
              selected = "A"),
  
  checkboxGroupInput( 
    inputId = "parameter", 
    label = h5("Oceanographic Parameters"), 
    c( 
      "Temperature (°C)" = "temp", 
      "PAR" = "PAR", 
      "Pressure (dbar)" = "pressure",
      "Oxygen (umol/kg)" = "oxygen_conc",
      "Oxygen (% saturation)"= "perc_ox_sat",
      "Fluorescence" = "fluorescence"))),
  
  column(9,
         plotOutput('profile'))),
  
  # 2nd row displays the FCM data, microbial profiles
  
  h3("🔬 Microbial Profile 🦠"),
  
  fluidRow(
    column(3,
           
           selectInput(inputId = "site", 
                       label = h5("Select a Station"), 
                       choices = unique(sort(fcm$site)), 
                       selected = "A"),
           
           checkboxGroupInput( 
             inputId = "microbe", 
             label = h5("Microbe Type"), 
             c( 
               "Prochlorococcus" = "pro_rel_abun",
               "Synechococcus" = "syn_rel_abun",
               "Pico Eukaryote" = "pico_euk_rel_abun",
               "Heterotrophic Bacteria" = "hbac_rel_abun"
               ))),
    
    column(9, 
           plotOutput("microbe_plot"))),
  
  # 3rd row displays the CTD and FCM data, correlations btwn both
  h3("📈 Correlation between Physics and Biology 📉"),
  
  fluidRow(
    column(3,
           
           selectInput(inputId = "parameter_corr", 
                       label = h5("Select a Parameter"), 
                       choices = unique(sort(CTD_FCM$parameter)), 
                       selected = "pressure"),
           
           selectInput(inputId = "microbe_corr", 
                       label = h5("Select a Microbe"), 
                       choices = unique(sort(CTD_FCM$microbe)), 
                       selected = "Prochlorococcus")),
    
    column(9, 
           plotOutput("correlation_plot")))
  
  
  )
  
 
  
# Define server 
server <- function(input, output) {

## CTD Graph
  CTD_data <- reactive({
    CTD %>% 
      mutate(station = as.factor(station)) %>% 
      filter(station == input$station)
      
  })
  
  output$profile <- renderPlot({
    
    profile <- CTD_data() %>%
      ggplot(aes(x = depth_cat* -1)) 
    
    # If no check boxes are selected
    if (is.null(input$parameter)) {
      profile <- ggplot(data = data.frame(x = 1, y = 1)) +
        geom_blank() + coord_flip()
      
    } else {
      # Define color mapping for each parameter
      color_mapping <- c(
        "temp" = "#FF9800",
        "PAR" = "#FFE082",
        "pressure" = "#607D8B",
        "oxygen_conc" = "#4DB6AC",
        "perc_ox_sat" = "#4DD0E1",
        "fluorescence" = "#4CAF50"
      )
      
      # Loop through selected parameters and add layers dynamically
      for (param in input$parameter) {
        profile <- profile +
          geom_point(aes_string(y = param), color = color_mapping[[param]]) +
          geom_line(aes_string(y = param), color = color_mapping[[param]])
      }
    }
    
    # Return the plot
    profile <- profile + 
      labs( title= "Oceanographic Profile",
          x = "Depth (m)",
          y = "Variables") +
      coord_flip()+
      theme_bw() + 
      theme(plot.title = element_text(size=14, face = "bold"), 
            plot.subtitle = element_text(size=12),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),
            strip.text.x = element_text(size = 12, face = "bold"),
            legend.title=element_text(size=12, face = "bold"),
            legend.text=element_text(size=12),
            panel.background = element_rect(fill = "azure1"))
    
    profile
  })
  
  ## FCM profile graph

  FCM_data <- reactive ({
    fcm %>% 
      mutate(depth = as.numeric(depth)) %>% 
      mutate(site = as.factor(site)) %>% 
      filter(site == input$site)
    
  })
  
  output$microbe_plot <- renderPlot({
    
    microbe_plot <- FCM_data() %>%
      ggplot(aes(x = depth * -1)) 
    
    # If no check boxes are selected
    if (is.null(input$microbe)) {
      microbe_plot <- ggplot(data = data.frame(x = 1, y = 1)) +
        geom_blank() + coord_flip()
      
    } else {
      # Define color mapping for each parameter
      color_mapping <- c(
        "pro_rel_abun" = "#66BB6A",
        "syn_rel_abun" = "#2E7D32",
        "pico_euk_rel_abun" = "#0288D1",
        "hbac_rel_abun" = "#673AB7"
      )
      
      # Loop through selected parameters and add layers dynamically
      for (param in input$microbe) {
        microbe_plot <- microbe_plot +
          geom_point(aes_string(y = param), color = color_mapping[[param]]) +
          geom_line(aes_string(y = param), color = color_mapping[[param]])
      }
    }
    
    # Return the plot
    microbe_plot <- microbe_plot + 
      labs( title = "Microbial Profile",
            x = "Depth (m)",
            y = "Relative Abundance (%)") +
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      theme_bw() + 
      theme(plot.title = element_text(size=14, face = "bold"), 
            plot.subtitle = element_text(size=12),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),
            strip.text.x = element_text(size = 12, face = "bold"),
            legend.title=element_text(size=12, face = "bold"),
            legend.text=element_text(size=12),
            panel.background = element_rect(fill = "azure1"))
    
      microbe_plot
  })
  
# correlation graph
  CTD_FCM_data <- reactive({
    CTD_FCM %>% 
      filter(microbe == input$microbe_corr) %>% 
      filter(parameter == input$parameter_corr)
    })
  
  output$correlation_plot <- renderPlot({
    
    correlation <- CTD_FCM_data()  %>% 
      ggplot(aes(x= values, y = rel_abun)) +
      geom_point()+
      geom_smooth(method = "lm")+
      labs(title = paste0("Correlation between ", input$parameter_corr,
                          " and ", input$microbe_corr),
           x = paste0(input$parameter_corr),
           y= paste0(input$microbe_corr, " (% abundance)"))+
      scale_y_continuous(labels = scales::percent)+
      theme_bw() + 
      theme(plot.title = element_text(size=16, face = "bold"), 
            plot.subtitle = element_text(size=15), 
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 14),
            legend.position = "none",
            panel.background = element_rect(fill = "azure1"))
    
    correlation
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
