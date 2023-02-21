

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
      filter(!is.na(genero)) |> 
      filter(!is.na(fecha)) |> 
      mutate(
        fecha = as.Date(fecha),
        mes = lubridate::month(fecha),
        anio = lubridate::year(fecha),
        periodo = paste(anio, mes, sep = "-")) |> 
      select(-c(mes, anio))
    
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
  
  
  # Lista que suplanta al parmesan_input() 
  list_inputs <- reactive({
    list(
      "Departamento" = input$departamentoId,
      "Género" = input$generoId
    )
    
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                  PASO III                                ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ### Base reactiva en función de filtros
  # data_filter <- reactive({
  #   req(data_load())
  #   df <- data_load()
  #   
  #   depto <- input$departamentoId
  #   genero_sel <- input$generoId
  #   
  #   #print(genero_sel)
  #   if (!is.null(depto)) df <- df |> filter(departamento %in% depto)
  #   if (!is.null(genero_sel)) df <- df |> filter(genero %in% genero_sel)
  #   df
  # })
  
  #### FILTRADO CON dsapptools
  dic_load <- reactive({
    
    dic <- homodatum::create_dic(data.table::fread("data/lideres.csv"))
    dic
  })
  
  
  data_down <- reactive({
    req(parmesan_input())
    ls <- parmesan_input()
    
    df <- dsapptools::data_filter(data = data_load(), 
                                  dic = dic_load(), 
                                  var_inputs = ls,
                                  special_placeholder = NULL,
                                  .id = "id")
    
    df
  })
  
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ##                                  PASO IV                                 ----
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ### Etapa de filtrado
  
  # data_viz <- reactive({
  #   
  #   req(data_down())
  #   req(input$viz_selection) 
  #   
  #   df <- data_down()
  #   
  #   if(input$viz_selection == "map") {
  #     df <- df |> select(departamento)
  #   }
  #   
  #   if(input$viz_selection == "bar") {
  #     df <- df |> select(genero)
  #   }
  #   
  #   if(input$viz_selection == "line") {
  #     df <- df |> select(periodo)
  #   }
  #   
  #   df
  #   
  # })
  
  var_to_viz <- reactive({
    req(input$viz_selection)
    
    var <- NULL
    
    if(input$viz_selection %in% c("map")) var <- "Departamento"
    
    if(input$viz_selection %in% c("bar")) var <- "Género"
    
    if(input$viz_selection %in% c("line")) var <- "periodo"
    
    var
  })
  
  data_viz <- reactive({
    
    if(input$viz_selection == "table") return()
    
    req(data_down())
    req(var_to_viz())
    
    if(nrow(data_viz()) == 0)return()
    
    df <- as_tibble(data_down())
    
    df <- df |> 
      dsapptools::variable_selection(viz = input$viz_selection, 
                                     path = NULL, var_to_viz())
    
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
      viz <- df |> 
        select(genero) |> 
        ggplot() +
        geom_bar(aes(x = genero), 
                       stat = "count")
      
    } else if(input$viz_selection == "table"){
      viz <- DT::datatable(df)
      
    } else if(input$viz_selection == "line"){
      viz <- df |> 
        count(periodo) |>  
        ggplot() +
        geom_line(aes(x = periodo, y = n, group = ""))
    }
    
    viz
    
  })
  
  
  
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ##                                  PASO VI                                 ----
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # ### Renderizo los gráficos
  
  # output$lflt_viz <- leaflet::renderLeaflet({
  #   if(input$viz_selection != "map") return()
  #   viz_save()
  # })
  # 
  # output$bar_viz <- renderPlot({
  #   if(input$viz_selection != "bar") return()
  #   viz_save()
  # })
  # 
  # output$table_viz <- DT::renderDT({
  #   if(input$viz_selection != "table") return()
  #   viz_save()
  # })
  # 
  # output$line_viz <- renderPlot({
  #   if(input$viz_selection != "line") return()
  #   viz_save()
  # })
  
  output$test <- renderPrint({
    data_viz()
  })
  
  
  # Genero Output a partir del render creado anteriormente
  # output$viz <- renderUI({
  # 
  #   if(is.null(input$viz_selection)) return()
  # 
  #   if(input$viz_selection == "map"){
  #     leaflet::leafletOutput("lflt_viz")
  # 
  #   } else if(input$viz_selection == "bar"){
  #     plotOutput("bar_viz")
  # 
  #   } else if(input$viz_selection == "table"){
  #     DT::DTOutput("table_viz")
  # 
  #   } else if(input$viz_selection == "line"){
  #     plotOutput("line_viz")
  #   }
  # 
  # })
  # 
  
  output$viz <- renderUI({
    verbatimTextOutput("test")
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
