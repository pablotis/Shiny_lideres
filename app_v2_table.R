
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              GENERAL SETTINGS                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Libraries
library(shiny)
library(shinyinvoer)
library(shinypanels)
library(dplyr)

### Dataset
data <- data.table::fread("data/lideres.csv")

data$genero <- as.factor(data$genero)
data$departamento <- as.factor(data$departamento)

# Choices for UI inputs
ch_genero <- unique(data$genero[!is.na(data$genero)])
ch_depto <- unique(data$departamento[!is.na(data$departamento)])

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
          uiOutput("select_depto")
        ),
        width = 350,
        footer = NULL),
  
  panel(title = "Visualización",
        color = "green",
        collapsed = FALSE,
        body = shiny::tableOutput("table"))
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
    
    selectizeInput(inputId = "genero", "Variable",
                   choices = ch_genero)
  })
  
  output$select_depto <- renderUI({
    
    selectizeInput(inputId = "depto", "Variable",
                   choices = ch_depto)
  })

### Salida de la tabla - Referencia al body del panel 2 (table)
  output$table <- renderTable({
    
    data |>
      select(nombre, fecha, movil, genero, departamento) |> 
      filter(genero == input$genero,
             departamento == input$depto)
  })
    
}

shinyApp(ui, server)
