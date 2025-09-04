library(shiny)
library(DT)
library(readxl)
library(dplyr)
library(KeyboardSimulator)

ui <- fluidPage(
  titlePanel("Envío masivo de WhatsApp"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Subir base (xlsx o csv)", 
                accept = c(".xlsx", ".csv")),
      textAreaInput("mensaje", "Escribe el mensaje:", 
                    "Hola\nmensaje de prueba\nmensaje de prueba 2", rows = 5),
      actionButton("enviar", "Enviar mensajes")
    ),
    
    mainPanel(
      h4("Base con progreso de envíos"),
      DTOutput("tabla")
    )
  )
)

server <- function(input, output, session) {
  datos <- reactiveVal(NULL)
  
  # Cuando el usuario sube la base
  observeEvent(input$file, {
    ext <- tools::file_ext(input$file$name)
    if (ext == "xlsx") {
      df <- read_excel(input$file$datapath)
    } else {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }
    
    # Agregamos columna Estado
    df <- df %>% mutate(Estado = "Pendiente")
    datos(df)
  })
  
  # Cuando se presiona el botón enviar
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
          mouse.move(2494,1319)  # ⚠️ Ajusta estas coordenadas a tu pantalla
          Sys.sleep(3)
          keybd.press("enter")
          Sys.sleep(2)
          keybd.press("Ctrl+w")
          Sys.sleep(2)
          
          df$Estado[i] <- "Enviado ✅"
        }, error = function(e) {
          df$Estado[i] <- "Error ❌"
        })
        
        datos(df)  # Actualizamos la tabla reactiva
      }
    })
  })
  
  output$tabla <- renderDT({
    req(datos())
    datatable(datos(), options = list(pageLength = 5))
  })
}

shinyApp(ui, server)
