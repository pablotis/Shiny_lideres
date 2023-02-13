

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
          # Acá la sección con tres iconitos
          uiOutput("viz_icons"),
          # Acá armo esqueleto para la sección de controles
          uiOutput("controls")
        ),
        width = 350,
        footer = NULL),
  
  panel(title = "Visualización",
        color = "green",
        collapsed = FALSE,
        body = div(
          # Acá sección donde van a ir las visualizaciones, en función de los iconitos
          verbatimTextOutput("test")
          #uiOutput("viz")
        )
        #body = textOutput("barplot"))
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
      # Con la función puedo armar el juego de los tres iconitos y que sean reactivos
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
    print(genero_sel)
    if (!is.null(depto)) df <- df |> filter(departamento %in% depto)
    if (!is.null(genero_sel)) df <- df |> filter(genero %in% genero_sel)
    df
  })


  output$test <- renderPrint({
    data_filter()
  })
  # 
  # 
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ##                                  PASO IV                                 ----
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # # Datos para visualizar
  # 
  # data_viz <- reactive({
  #   
  #   req(data_filter())
  #   req(input$viz_selection) # Este es el esqueleto de los iconitos
  #   
  #   df <- data_filter()
  #   
  #   if(input$viz_selection == "map") {
  #     df <- df |> select(departamento)
  #   }
  # 
  # })
  # 
  # 
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ##                                   PASO V                                 ----
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # # Gráficos
  # 
  # viz_save <- reactive({
  #   req(data_viz())
  #   
  #   df <- data_viz()
  #   
  #   ## Salida para mapa
  #   if(input$viz_selection == "map"){
  #     viz <- lfltmagic::lflt_choropleth_Gnm(df, map_name = "col_larg")
  #     
  #   }
  #   
  #   viz
  #   
  # })
  # 
  # 
  # 
  # 
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ##                                  PASO VI                                 ----
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # ### Renderizo los gráficos
  # 
  # output$lflt_viz <- leaflet::renderLeaflet({
  #   if(input$viz_selection != "map") return()
  #   viz_save()
  # })
  # 
  # 
  # # Genero Output a partir del render creado anteriormente
  # output$viz <- renderUI({
  #   
  #   if(is.null(input$viz_selection)) return()
  #   
  # 
  #   if(input$viz_selection == "map"){
  #     leaflet::leafletOutput("lflt_viz")
  #   }
  #     
  # 
  #   })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
