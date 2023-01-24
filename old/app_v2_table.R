
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
          uiOutput("select_depto"),
          uiOutput("select_date")
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
  

  #..................Selector para variable genero.................
  output$select_genero <- renderUI({
    
    selectizeInput(inputId = "opc_genero", "Variable",
                   choices = ch_genero)
  })

  #..............Selector para variable departamento...............
  output$select_depto <- renderUI({
    
    selectizeInput(inputId = "opc_depto", "Variable",
                   choices = ch_depto)
  })
  
  
  #..................Selector para variable fecha..................
  output$select_date <- renderUI({
    shinyinvoer::dateRangeInput(
      inputId = "date_range",
      label = 'Seleccione el rango de fechas',
      start = '2016-01-02',
      end = '2020-06-31',
      min = '2016-01-02',
      max = '2020-06-31',
      startLabel = 'Fecha inicial',
      endLabel = 'Fecha final',
      resetLabel = 'Restablecer fechas',
      locale = 'es'
    )
  })
  
  # output$the_output <- renderPrint({
  #   input$date_range
  # })
  
  
  ### Salida de la tabla - Referencia al body del panel 2 (table)
  output$table <- renderTable({
    
    data |>
      select(nombre, fecha, movil, genero, departamento) |> 
      filter(genero == input$opc_genero,
             departamento == input$opc_depto,
             !is.na(fecha),
             (fecha >= input$date_range$startLabel & fecha <= input$date_range$endLabel))
  })
    
}

shinyApp(ui, server)
