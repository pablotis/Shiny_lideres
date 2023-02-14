

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

#.............................UI.............................

### Defino los paneles unicamente en la UI.


ui <- panelsPage(
  
  ### Con {parmesan} puedo armar cuántos paneles quiero y definir el esqueleto del contenido hacia el interior
  panel(title = "Acá van los filtros",
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
        body = div(
          uiOutput("viz")
        )
  )
)




#.............................SERVER.............................

server <- function(input, output, session) {
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                   PASO I                                 ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Defino los iconitos primero: para barras, mapa y tabla
  output$viz_icons <- renderUI({
    
    viz <- c("map", "bar", "table", "line")
    viz_label <- c("Map", "Bar", "Table", "Line")
    
    
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
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                  PASO II                                 ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ### Cargo base de datos en versión "reactiva"
  data_load <- reactive({
    df <- data.table::fread("data/lideres.csv") |> 
      filter(!is.na(genero))
    
    df
  })
  
  # Defino de forma reactiva las diferentes opciones de departamentos a seleccionar en filtros
  dept_opc <- reactive({
    req(data_load())
    unique(data_load()$departamento)
  })
  
  genero_opc <- reactive({
    req(data_load())
    unique(data_load()$genero)
  })
  
  
  ### Seteo parmesan para definición de filtros
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  
  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())
  
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                  PASO III                                ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ### Base reactiva en función de filtros
  data_filter <- reactive({
    req(data_load())
    df <- data_load()
    
    depto <- input$departamentoId
    genero_sel <- input$generoId
    
    #print(genero_sel)
    if (!is.null(depto)) df <- df |> filter(departamento %in% depto)
    if (!is.null(genero_sel)) df <- df |> filter(genero %in% genero_sel)
    df
  })
  
  
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ##                                  PASO IV                                 ----
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ### Etapa de filtrado
  
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
  
  
  ### Etapa de selección de columnas para gráficos
  viz_save <- reactive({
    req(data_viz())
    
    df <- data_viz()
    
    ## Salida para mapa
    if(input$viz_selection == "map"){
      viz <- lfltmagic::lflt_choropleth_Gnm(df, map_name = "col_larg")
    } else if(input$viz_selection == "bar"){
      #viz <- highcharter::highchart(df)
      viz <- df |> 
        select(genero) |> 
        ggplot() +
        geom_bar(aes(x = genero), 
                       stat = "count")
    }
    
    viz
    
  })
  
  
  
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ##                                  PASO VI                                 ----
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # ### Renderizo los gráficos
  
  output$lflt_viz <- leaflet::renderLeaflet({
    if(input$viz_selection != "map") return()
    viz_save()
  })
  
  # output$bar_viz <- highcharter::renderHighchart({
  #   if(input$viz_selection != "bar") return()
  #   viz_save()
  # })
  
  output$bar_viz <- renderPlot({
    if(input$viz_selection != "bar") return()
    viz_save()
  })
  
  # output$test <- renderPrint({
  #   data_viz()
  # })
  
  
  ## Genero Output a partir del render creado anteriormente
  output$viz <- renderUI({
    
    if(is.null(input$viz_selection)) return()
    
    if(input$viz_selection == "map"){
      leaflet::leafletOutput("lflt_viz")
    } else if(input$viz_selection == "bar"){
      #highcharter::highchartOutput("bar_viz")
      plotOutput("bar_viz")
    }
    
  })
  
  
  # output$viz <- renderUI({
  #   verbatimTextOutput("test")
  # })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
