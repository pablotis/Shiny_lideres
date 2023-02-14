
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
library(leaflet)
library(makeup)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                               USER INTERFACE                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- panelsPage(
  
  panel(title = "Filtros",
        color = "lightgreen",
        collapsed = FALSE,
        body = div(
          uiOutput("viz_icons"),
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
  
  
  
  output$viz_icons <- renderUI({
    viz <- c("map", "bar", "table")
    viz_label <- c("Map", "Bar", "Table")
    
    
    suppressWarnings(
      shinyinvoer::buttonImageInput("viz_selection",
                                    " ",
                                    images = viz,
                                    tooltips = viz_label,
                                    path = "img/viz_icons/",
                                    imageStyle = list(shadow = TRUE,
                                                      borderColor = "#ffffff",
                                                      padding = "3px")
      )
    )
  })
  
  ### Cargo base
  data_load <- reactive({
    df <- data.table::fread("data/lideres.csv")
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
    req(input$viz_selection)
    
    df <- data_filter()
    
    if(input$viz_selection == "map") {
      df <- df |> select(departamento)
    }
    if(input$viz_selection == "bar") {
      df <- df |> select(genero)
    }
    df
    
  })
  
  # Gráficos
  
  viz_save <- reactive({
    req(data_viz())
    print(data_viz())
    df <- data_viz()
    
    ## Salida para Tabla
    if(input$viz_selection == "table") return()
    
    ## Salida para mapa
    if(input$viz_selection == "map"){
      viz <- lfltmagic::lflt_choropleth_Gnm(df, map_name = "col_larg")
    }
    ## Salida para barras
    if(input$viz_selection == "bar"){
      viz <- ggplot2::ggplot(df) + geom_bar()
    }
    viz
  })
  
  
  ### Renderizo los gráficos
  output$hgch_viz <- highcharter::renderHighchart({
    if(input$viz_selection != "bar") return()
    viz_save()
  })
  
  output$lflt_viz <- leaflet::renderLeaflet({
    if(input$viz_selection != "map") return()
    viz_save()
  })
  
  output$table <- DT::renderDataTable({
    req(data_filter())
    data_filter()
  })
  
  
  
  ### Convoco los gráficos renderizados
  output$viz <- renderUI({
    #req(input$viz_selection)
    
    if(input$viz_selection == "bar"){
      highcharter::highchartOutput("hgch_viz")
      
    } else if(input$viz_selection == "map"){
      leaflet::leafletOutput("lflt_viz")
      
    } else {
      DT::dataTableOutput("table")
    }
  })
  

}



shinyApp(ui, server)
