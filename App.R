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
    .well {
      background-color: #1a1c3a !important;
      color: #E0E0E0 !important;
      border: 1px solid #E0E0E0;
    }
    .btn, .form-control {
      background-color: #010214ff !important;
      color: #E0E0E0 !important;
      border: 1px solid #E0E0E0 !important;
    }
    .btn:hover {
      background-color: #394080 !important;
      color: white !important;
    }
    table.dataTable {
      background-color: #010214ff !important;
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
      fileInput("file", "Subir base (xlsx o csv)", 
                accept = c(".xlsx", ".csv")),
      textAreaInput("mensaje", "Escribe el mensaje:", 
                    "Hola\nmensaje de prueba\nmensaje de prueba 2", rows = 5),
      actionButton("enviar", "Enviar mensajes", class = "btn-success")
    ),
    
    mainPanel(
      h4("Base con progreso de envíos"),
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
    
    df <- df %>% mutate(Estado = "Pendiente")
    datos(df)
  })
  
  observeEvent(input$enviar, {
    req(datos())
    df <- datos()
    
    withProgress(message = "Enviando WhatsApp...", value = 0, {
      for (i in 1:nrow(df)) {
        incProgress(1/nrow(df), detail = paste("Enviando a", df$WhatsApp[i]))
        
        celular <- as.character(df$WhatsApp[i])
        mensaje_url <- URLencode(gsub("\n", "%0A", input$mensaje))
        urlwa <- paste0("https://web.whatsapp.com/send?phone=", celular, "&text=", mensaje_url)
        
        tryCatch({
          browseURL(urlwa)
          Sys.sleep(7)
          Sys.sleep(3)
          system('powershell -command "$wshell = New-Object -ComObject wscript.shell; $wshell.SendKeys(\'{ENTER}\')"')
          Sys.sleep(2)
          system('powershell -command "$wshell = New-Object -ComObject wscript.shell; $wshell.SendKeys(\'^w\')"')

          Sys.sleep(2)
          
          df$Estado[i] <- "Enviado ✅"
        }, error = function(e) {
          df$Estado[i] <- "Error ❌"
        })
        
        datos(df)
      }
    })
  })
  
  output$tabla <- renderDT({
    req(datos())
    datatable(datos(), options = list(pageLength = 5))
  })
}




shinyApp(ui, server)
