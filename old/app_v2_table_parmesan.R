
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
        id = "filtros",
        color = "lightgreen",
        collapsed = FALSE,
        body = div(
          uiOutput("generoId"),
          uiOutput("departamentoId"),
          #uiOutput("fechaId")
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
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())
  
  
  opt_genero <- reactive({
    ch_genero
  })
  
  opt_departamento <- reactive({
    ch_departamento
  })
  
  # opt_fecha <- reactive({
  #   ch_genero
  # })
  
  
  output$filtros <- renderUI({
    render_section(parmesan = parmesan)
  })
  
  # #..................Selector para variable fecha..................
  # sel_date_mean_opts <- reactive({
  #   req(sel_date_min_opts())
  #   req(sel_date_max_opts())
  #   c(sel_date_min_opts(),  sel_date_max_opts())
  # })
  # 
  # sel_date_min_opts <- reactive({
  #   min(data$fecha, na.rm=TRUE)
  # })
  # 
  # sel_date_max_opts <- reactive({
  #   max(data$fecha, na.rm=TRUE)
  # })
  # 
  # 
  # 
  # 
  # #..................Selector para variable genero.................
  # output$select_genero <- renderUI({
  #   
  #   selectizeInput(inputId = "opc_genero", "Variable",
  #                  choices = ch_genero)
  # })
  # 
  # #..............Selector para variable departamento...............
  # output$select_depto <- renderUI({
  #   
  #   selectizeInput(inputId = "opc_depto", "Variable",
  #                  choices = ch_depto)
  # })
  
  
  
  
  # output$the_output <- renderPrint({
  #   input$date_range
  # })
  
  
  ### Salida de la tabla - Referencia al body del panel 2 (table)
  output$table <- renderTable({
    
    
    
    data |>
      select(nombre, fecha, movil, genero, departamento) |> 
      filter(genero == input$opc_genero,
             departamento == input$opc_depto,
             !is.na(fecha)
             #(fecha >= input$date_range$startLabel & fecha <= input$date_range$endLabel)
             )
  })
    
}

shinyApp(ui, server)
