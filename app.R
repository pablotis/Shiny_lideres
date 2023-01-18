
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              GENERAL SETTINGS                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Libraries
library(shiny)
library(shinyinvoer)
library(shinypanels)

### Dataset
data <- data.table::fread("data/lideres.csv")

data$genero <- as.factor(data$genero)
data$departamento <- as.factor(data$departamento)

# Choices for UI inputs
ch_genero <- unique(data$genero[!is.na(data$genero)])
ch_departamento <- unique(data$departamento[!is.na(data$departamento)])

ch_variable <- c("genero", "departamento")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                               USER INTERFACE                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- panelsPage(

  panel(title = "Filtros",
        color = "lightgreen",
        collapsed = FALSE,
        body = div(
          uiOutput("select_genero"),
          uiOutput("select_tipo_lider")
        ),
        width = 350,
        footer = NULL),
  
  panel(title = "Visualización",
        color = "green",
        collapsed = FALSE,
        body = plotOutput("barplot"))
        #body = textOutput("barplot"))
)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   SERVER                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {
  
  opc_variables <- reactive({
    ch_variable <- c("genero", "departamento")
  })
  

### Salida del panel de seleccion
  output$select_genero <- renderUI({
    
    selectizeInput(inputId = "variable", "Variable",
                   choices = ch_variable)
  })

### Salida del gráfico - Referencia al body del panel 2 (barplot)
  output$barplot <- renderPlot({
    
    ggplot(data, aes(.data[[input$variable]])) + 
      geom_bar()
    
  })
  
  
### Salida del panel de seleccion
  output$select_variable <- renderUI({
    
    selectizeInput(inputId = "variable", "Variable",
                   choices = ch_variable)
  })
  
}

shinyApp(ui, server)
