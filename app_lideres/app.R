#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  uiOutput("select"),
  plotOutput("graf")

) 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$select <- renderUI({
    
    sliderInput(inputId = "obs", 
                label = "Deslizador", 
                min = 10, 
                max = 100, 
                value = 50)
    })
  
  output$graf <- renderUI({
    
    hist()
  })
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
