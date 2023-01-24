
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              GENERAL SETTINGS                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Libraries
library(shiny)
library(shinyinvoer)
library(shinypanels)
library(parmesan)

### Dataset
data <- data.table::fread("data/lideres.csv")

data$genero <- as.factor(data$genero)
data$departamento <- as.factor(data$departamento)

# Choices for UI inputs
#ch_genero <- unique(data$genero[!is.na(data$genero)])
#ch_departamento <- unique(data$departamento[!is.na(data$departamento)])



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                               USER INTERFACE                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- panelsPage(
  
  shinypanels::panel(title = "Filtros",
        color = "lightgreen",
        collapsed = FALSE,
        body = div(
          shiny::uiOutput("controls")
        ),
        width = 350,
        footer = NULL),
  
  shinypanels::panel(title = "Visualización",
        color = "green",
        collapsed = FALSE,
        body = shiny::plotOutput("viz"))
  #body = textOutput("viz"))
)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   SERVER                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {
  
  opc_variables <- reactive({
    ch_variable <- c("genero", "departamento")
  })
  
  # opciones para parmesan ------------------------------------------
  
  pickerOpts <- reactive({
    list(
      `actions-box` = TRUE,
      `deselect-all-text` = "Ninguno",
      `select-all-text` = "Todos",
      title = "Todos"
    )
  })
  
  ch_genero <- reactive({
    unique(data$genero)
  })
  
  ch_tipo_lider <- reactive({
    unique(data$tipo_lider)
  })
  
  ch_departamento <- reactive({
    unique(data$departamento)
  })
  
  # Renderizar inputs con parmesan ------------------------------------------
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  
  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())
  
  
  # Filtrar datos ------------------------------------------
  
  
  d_filter <- reactive({
    ls <- parmesan_input()
    # req(reactiveValuesToList(input))
    ls <- ls[c("genero", "tipo_lider", "departamento")]
    names(ls) <- c("Género", "Tipo de líder", "Departamento")
    inpandetec::data_filter(data, ls)
  })
  
}

shinyApp(ui, server)
