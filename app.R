#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(readxl)
library(writexl)
library(dplyr)
library(DT)

# Función para guardar el log de cambios
registrar_log <- function(id_valor, columna_id, nuevos_datos, archivo_original) {
  log_path <- "log_ediciones.txt"
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  texto_log <- paste0(
    "\n[", timestamp, "]\n",
    "Archivo cargado: ", archivo_original, "\n",
    "Columna ID: ", columna_id, "\n",
    "ID modificado: ", id_valor, "\n",
    "Valores actualizados:\n"
  )
  
  for (campo in names(nuevos_datos)) {
    texto_log <- paste0(texto_log, "  - ", campo, ": ", nuevos_datos[[campo]], "\n")
  }
  
  cat(texto_log, file = log_path, append = TRUE)
}

ui <- fluidPage(
  titlePanel("Editor dinámico de base de datos desde Excel (.xlsx)"),
  
  sidebarLayout(
    sidebarPanel(
      div(style = "height: 600px; overflow-y: auto;",
          fileInput("archivo", "Cargar archivo Excel (.xlsx)", accept = ".xlsx"),
          uiOutput("id_col_selector"),
          uiOutput("id_value_selector"),
          actionButton("cargar", "Cargar datos del paciente"),
          tags$hr(),
          uiOutput("formulario_dinamico"),
          actionButton("guardar", "Guardar/Actualizar"),
          downloadButton("descargar", "Descargar base modificada"),
          downloadButton("descargar_log", "Descargar log de cambios"),
          verbatimTextOutput("mensaje")
      )
    ),
    
    mainPanel(
      h4("Vista de la base de datos:"),
      div(style = "overflow-x: auto; overflow-y: auto; max-height: 600px;",
          DTOutput("tabla_base")
      )
    )
  )
)

server <- function(input, output, session) {
  datos <- reactiveVal(NULL)
  nombres_columnas <- reactiveVal(NULL)
  
  # Cargar archivo
  observeEvent(input$archivo, {
    req(input$archivo)
    ext <- tools::file_ext(input$archivo$name)
    validate(need(ext == "xlsx", "El archivo debe ser .xlsx"))
    
    base <- read_xlsx(input$archivo$datapath)
    base <- as.data.frame(base)
    datos(base)
    nombres_columnas(names(base))
  })
  
  # Selector de columna que actuará como ID
  output$id_col_selector <- renderUI({
    req(nombres_columnas())
    selectInput("columna_id", "Selecciona la columna identificadora (ID):",
                choices = nombres_columnas(),
                selected = nombres_columnas()[1])
  })
  
  # Selector del valor dentro de la columna ID
  output$id_value_selector <- renderUI({
    req(input$columna_id, datos())
    selectizeInput("id_valor",
                   paste("Selecciona un valor de ID en:", input$columna_id),
                   choices = unique(datos()[[input$columna_id]]),
                   selected = NULL,
                   options = list(placeholder = 'Escribe o selecciona un ID'),
                   multiple = FALSE)
  })
  
  # Formulario dinámico
  output$formulario_dinamico <- renderUI({
    req(nombres_columnas())
    lapply(nombres_columnas()[nombres_columnas() != input$columna_id], function(col) {
      tipo <- class(datos()[[col]])[1]
      if (tipo %in% c("numeric", "integer")) {
        numericInput(inputId = col, label = col, value = NA)
      } else {
        textInput(inputId = col, label = col, value = "")
      }
    })
  })
  
  # Tabla interactiva
  output$tabla_base <- renderDT({
    req(datos())
    datatable(datos(),
              options = list(scrollX = TRUE, pageLength = 10),
              rownames = FALSE)
  })
  
  # Cargar datos por ID seleccionado
  observeEvent(input$cargar, {
    req(datos(), input$columna_id, input$id_valor)
    df <- datos()
    id_col_data <- df[[input$columna_id]]
    
    fila <- df[as.character(id_col_data) == as.character(input$id_valor), ]
    
    if (nrow(fila) == 1) {
      for (col in nombres_columnas()[nombres_columnas() != input$columna_id]) {
        valor <- fila[[col]]
        if (is.numeric(valor)) {
          updateNumericInput(session, col, value = valor)
        } else {
          updateTextInput(session, col, value = as.character(valor))
        }
      }
      output$mensaje <- renderText("✔ Datos cargados.")
    } else {
      output$mensaje <- renderText("❌ ID no encontrado.")
    }
  })
  
  # Guardar datos actualizados
  observeEvent(input$guardar, {
    req(datos(), input$columna_id, input$id_valor)
    df <- datos()
    
    nuevo <- setNames(vector("list", length(nombres_columnas())), nombres_columnas())
    
    if (is.numeric(df[[input$columna_id]])) {
      nuevo[[input$columna_id]] <- as.numeric(input$id_valor)
    } else {
      nuevo[[input$columna_id]] <- as.character(input$id_valor)
    }
    
    for (col in nombres_columnas()[nombres_columnas() != input$columna_id]) {
      val <- input[[col]]
      if (is.numeric(df[[col]])) {
        val <- as.numeric(val)
      }
      nuevo[[col]] <- val
    }
    
    nueva_fila <- as.data.frame(nuevo, stringsAsFactors = FALSE)
    
    df <- df[as.character(df[[input$columna_id]]) != as.character(nuevo[[input$columna_id]]), ]
    df <- bind_rows(df, nueva_fila)
    datos(df)
    
    updateSelectizeInput(session, "id_valor",
                         choices = sort(unique(df[[input$columna_id]])),
                         selected = "",
                         server = TRUE)
    
    registrar_log(
      id_valor = nuevo[[input$columna_id]],
      columna_id = input$columna_id,
      nuevos_datos = nuevo,
      archivo_original = input$archivo$name
    )
    
    for (col in nombres_columnas()[nombres_columnas() != input$columna_id]) {
      if (is.numeric(df[[col]])) {
        updateNumericInput(session, col, value = NA)
      } else {
        updateTextInput(session, col, value = "")
      }
    }
    
    output$mensaje <- renderText("✅ Datos guardados o actualizados.")
  })
  
  # Descargar archivo modificado
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("base_modificada_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(datos())
      write_xlsx(datos(), path = file)
    }
  )
  
  # Descargar log de cambios
  output$descargar_log <- downloadHandler(
    filename = function() {
      paste0("log_ediciones_", Sys.Date(), ".txt")
    },
    content = function(file) {
      file.copy("log_ediciones.txt", file)
    }
  )
}

shinyApp(ui, server)
