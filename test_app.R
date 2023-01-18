
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
ch_genero <- unique(data$genero[!is.na(data$genero)])
ch_departamento <- unique(data$departamento[!is.na(data$departamento)])



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
