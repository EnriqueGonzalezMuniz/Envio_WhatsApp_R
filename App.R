library(shiny)
library(DT)
library(readxl)
library(dplyr)

ui <- fluidPage(
  # 🎨 Estilos oscuros
  tags$style(HTML("
   body {
      background-color: #010214ff;
      color: #E0E0E0;
    }
    /* Fondo sidebar */
    .well {
      background-color: #1a1c3a !important;
      color: #E0E0E0 !important;
      border: 1px solid #E0E0E0;
    }
    /* Inputs y botones */
    .btn, .form-control {
      background-color: #010214ff !important;
      color: #E0E0E0 !important;
      border: 1px solid #E0E0E0 !important;
    }
    .btn:hover {
      background-color: #394080 !important;
      color: white !important;
    }
    /* DataTable: fondo oscuro y letras claras */
    table.dataTable {
      background-color: #010214ff7 !important;
      color: #E0E0E0 !important;
    }
    table.dataTable thead th {
      background-color: #1a1c3a !important;
      color: #E0E0E0 !important;
    }
    table.dataTable tbody tr {
      background-color: #010214ff !important;
      color: #E0E0E0 !important;
    }
    table.dataTable tbody tr.selected {
      background-color: #394080 !important;
      color: white !important;
    }
    .dataTables_wrapper .dataTables_filter input,
    .dataTables_wrapper .dataTables_length select {
      background-color: #1a1c3a !important;
      color: #E0E0E0 !important;
      border: 1px solid #E0E0E0 !important;
    }
  ")),
  
  titlePanel("Generador de enlaces de WhatsApp"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Subir base (xlsx o csv)", accept = c(".xlsx", ".csv")),
      textAreaInput("mensaje", "Escribe el mensaje:",
                    "Hola\nmensaje de prueba\nmensaje de prueba 2", rows = 5)
    ),
    mainPanel(
      h4("Base con enlaces de WhatsApp"),
      DTOutput("tabla")
    )
  )
)


server <- function(input, output, session) {
  datos <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    ext <- tools::file_ext(input$file$name)
    if (ext == "xlsx") {
      df <- read_excel(input$file$datapath)
    } else {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }
    datos(df)
  })
  
  output$tabla <- renderDT({
    req(datos())
    df <- datos()
    
    # Construir mensaje y link
    mensaje_url <- URLencode(gsub("\n", "%0A", input$mensaje))
    df <- df %>%
      mutate(Link = paste0(
        "https://wa.me/", WhatsApp, "?text=", mensaje_url
      ))
    
    # Hacer columna de link clickeable
    df$Accion <- paste0("<a href='", df$Link, 
                        "' target='_blank' class='btn'>Abrir chat</a>")
    
    datatable(df[, c(names(df)[!names(df) %in% "Link"], "Accion")],
              escape = FALSE,
              options = list(pageLength = 5))
  })
}

shinyApp(ui, server)
