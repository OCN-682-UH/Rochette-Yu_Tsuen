
# Title : First Shiny App
# Created by: Keanu Rochette
# Date Creation: 2024-11-12
############################

library(shiny)
library(tidyverse)


# Coding for the User Interface (UI)
## The order of code blocks matter ! They will appear in order of the code
ui <- fluidPage(
  sliderInput(inputId = "num", # ID name for the input
              label = "Choose a number", # Label above the input
              value = 25, min = 1, max = 100 # values for the slider
  ), # areas of code for the website are separated by commas
  
  textInput(inputId = "title", # ID for title to use later
            label = "Write a title", #label of the text block
            value = "Histogramme of Random Normal Values"), #default value
  
  plotOutput("hist"), #creates a space for a plot to appear on the website
  verbatimTextOutput("stats") #creates a space for stats
)



# Code for the guts and spine of the Shiny App
server <- function(input,output){
  
# This section is for data related stuff
  ## generate X random normal points based on the input num
  data <- reactive ({
    ### this is where you would load data and do all the data wrangling
  
    tibble(x = rnorm(input$num)) 
  })
  
  
  
# This section is for plotting: make the histogram in the final app
  output$hist <- renderPlot({
  
    #data is a FUNCTION, hence it requires the ()
    data() %>% ggplot(aes(x = x)) + # make a histogram
      geom_histogram() +
      labs(title = input$title) #add a new title that's interactive
    
  })

# Miscellaneous stuff of R code
  output$stats <- renderPrint({
    #data is a FUNCTION, hence it requires the ()
    summary(data()) #calculate the summary stats based on the number
  })
}


# Rendering the app
shinyApp(ui = ui, server = server)






























