
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              GENERAL SETTINGS                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Libraries
library(shiny)
library(shinyinvoer)
library(shinypanels)
library(parmesan)
library(dplyr)
library(readr)
library(lfltmagic)
library(hgchmagic)
library(DT)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                               USER INTERFACE                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- panelsPage(
  
  panel(title = "Filtros",
        color = "lightgreen",
        collapsed = FALSE,
        body = div(
          uiOutput("controls")
        ),
        width = 350,
        footer = NULL),
  
  panel(title = "Visualización",
        color = "green",
        collapsed = FALSE,
        body = uiOutput("viz"))
  #body = textOutput("barplot"))
)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   SERVER                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {
  
  
  ### Cargo base
  data_load <- reactive({
    df <- readr::read_csv("data/lideres.csv")
    df
  })
  
  # Opciones para input departamento
  dept_opc <- reactive({
    req(data_load())
    unique(data_load()$departamento)
  })
  
  ### Seteo parmesan
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  
  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())
  
  
  ### Base reactiva en función de filtros
  data_filter <- reactive({
    req(data_load())
    df <- data_load()
    
    print(input$departamentoId)
    
    if(is.null(input$departamentoId)){
      df <- df
    } else {
      df <- data_load() |>
        dplyr::filter(departamento %in% input$departamentoId)
    }
    df
  })
  
  
  # Datos para visualizar
  
  data_viz <- reactive({
    req(data_filter())
    
    df <- data_filter()|> select(genero)
    
    df
    
  })
  
  # Gráficos
  
  viz_save <- reactive({
    req(data_viz())
    df <- data_viz()
    
    viz <- hgchmagic::hgch_bar_Cat(df, map_name = "col_larg")
    
    viz
  })
  
  
  ### Renderizo los gráficos
  output$hgch_viz <- highcharter::renderHighchart({
    req(viz_save())
    viz_save()
  })
  
  
  
  ### Convoco los gráficos renderizados
  output$viz <- renderUI({
    
    highcharter::highchartOutput("hgch_viz")
    
  })
  
  
}



shinyApp(ui, server)
