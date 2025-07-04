library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(scales)  
library(lubridate)
library(stringr)
library(shinythemes) 

# --- UI ---
ui <- fluidPage(
  theme = shinytheme("flatly"), 
  
  navbarPage(
    title = "Ejecución Presupuestaria UNC", 
    
    tabPanel(
      "Tabla de datos", 
      sidebarLayout(
        sidebarPanel(
          h4("Filtros de Datos"),
          selectInput(
            inputId = "unidad_presupuestaria_select",
            label = "Seleccionar Unidad Presupuestaria:",
            choices = NULL, 
            multiple = TRUE,
            selected = NULL
          ),
          
          selectInput(
            inputId = "inciso_select",
            label = "Seleccionar Inciso:",
            choices = NULL, 
            multiple = TRUE,
            selected = NULL
          ),
          hr(),
          
          h4("Opciones de Visualización Gráfica"),
          radioButtons(
            inputId = "plot_group_by",
            label = "¿Agrupar gráfico por?",
            choices = c("Unidad Presupuestaria" = "Unidad_Presupuestaria",
                        "Inciso" = "Inciso"),
            selected = "Inciso"
          )
        ),
        
        mainPanel(
          textOutput("update_date_text"), 
          h3("Tabla de Ejecución"), 
          DT::dataTableOutput("ejecucion_table") 
        )
      )
    ),
    
    tabPanel("Gráfico", 
             plotlyOutput("ejecucion_plot", height = "90vh")
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  output$update_date_text <- renderText({
    formatted_date <- format(floor_date(Sys.Date(), "week"), "%d/%m/%Y")
    paste("Información actualizada al:", formatted_date)
  })
  
  datos <- read.csv("C:/Users/Usuario/Downloads/shiny_presupuesto.csv")
  
  datos <- rename(datos, Unidad_Presupuestaria = unidad_presupuestaria_desc,
                  Ejecutado = ejecutado,
                  Inciso = inciso)
  datos$Unidad_Presupuestaria <- str_replace(datos$Unidad_Presupuestaria, "^\\d+\\s+-\\s+(UNC\\s*)?", "")
  datos$Inciso <- str_replace(datos$Inciso, "^\\d+\\s+-\\s+(UNC\\s*)?", "")
  datos$Ejecutado <- as.numeric(datos$Ejecutado)
  datos = subset(datos, anio == year(Sys.Date()))
  
  observe({
    updateSelectInput(session, "unidad_presupuestaria_select",
                      choices = sort(unique(datos$Unidad_Presupuestaria)),
                      selected = unique(datos$Unidad_Presupuestaria))
    updateSelectInput(session, "inciso_select",
                      choices = sort(unique(datos$Inciso)),
                      selected = unique(datos$Inciso))
  })
  
  filtered_data_table <- reactive({
    req(input$unidad_presupuestaria_select, input$inciso_select)
    
    datos %>%
      filter(Unidad_Presupuestaria %in% input$unidad_presupuestaria_select) %>%
      filter(Inciso %in% input$inciso_select) %>%
      group_by(Unidad_Presupuestaria, Inciso) %>%
      summarise(
        Ejecutado_Total = sum(Ejecutado),
        .groups = 'drop'
      ) %>%
      mutate(
        Ejecutado_Millones_Display = paste0(
          "$",
          format(round(Ejecutado_Total / 1e6, 2), 
                 big.mark = ".",
                 decimal.mark = ","),
          " M"
        )
      ) %>%
      select(Unidad_Presupuestaria, Inciso, Ejecutado_Millones_Display) %>%
      arrange(Unidad_Presupuestaria, Inciso)
  })
  
  plot_data_aggregated <- reactive({
    req(input$unidad_presupuestaria_select, input$inciso_select)
    
    base_data <- datos %>%
      filter(Unidad_Presupuestaria %in% input$unidad_presupuestaria_select) %>%
      filter(Inciso %in% input$inciso_select)
    
    aggregated_data <- if (input$plot_group_by == "Unidad_Presupuestaria") {
      base_data %>%
        group_by(Unidad_Presupuestaria) %>%
        summarise(Ejecutado_Total_Plot = sum(Ejecutado), .groups = 'drop') %>%
        arrange(desc(Ejecutado_Total_Plot))
    } else {
      base_data %>%
        group_by(Inciso) %>%
        summarise(Ejecutado_Total_Plot = sum(Ejecutado), .groups = 'drop') %>%
        arrange(desc(Ejecutado_Total_Plot))
    }
    
    aggregated_data %>%
      mutate(
        tooltip_text = paste0(
          names(which(c("Unidad_Presupuestaria" = "Unidad_Presupuestaria",
                        "Inciso" = "Inciso") == input$plot_group_by)), ": ",
          .data[[input$plot_group_by]],
          "<br>Ejecutado Total: $",
          format(round(Ejecutado_Total_Plot / 1e6, 0),
                 big.mark = ".", decimal.mark = ",", scientific = FALSE),
          " Millones"
        )
      )
  })
  
  output$ejecucion_table <- DT::renderDataTable({
    table_data <- filtered_data_table()
    colnames(table_data)[colnames(table_data) == "Ejecutado_Millones_Display"] <- "Ejecutado (Millones de $)"
    
    datatable(table_data, options = list(
      pageLength = 15,
      lengthMenu = c(5, 10, 25, 50),
      dom = 'Bfrtip',
      buttons = list( # Definición explícita de los botones en español
        list(extend = 'copy', text = 'Copiar'),
        list(extend = 'csv', text = 'CSV'),
        list(extend = 'excel', text = 'Excel'),
        list(extend = 'pdf', text = 'PDF')
      ),
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json'
      )
    ), extensions = 'Buttons')
  })
  
  output$ejecucion_plot <- renderPlotly({
    plot_data <- plot_data_aggregated()
    
    x_axis_var <- switch(input$plot_group_by,
                         "Unidad_Presupuestaria" = "Unidad_Presupuestaria",
                         "Inciso" = "Inciso")
    
    p <- ggplot(plot_data, aes_string(x = x_axis_var, y = "Ejecutado_Total_Plot",
                                      text = "tooltip_text")) +
      geom_col(fill = "steelblue") +
      labs(
        title = paste("Ejecutado Total por", names(which(c("Unidad_Presupuestaria" = "Unidad_Presupuestaria", "Inciso" = "Inciso") == input$plot_group_by))),
        x = names(which(c("Unidad_Presupuestaria" = "Unidad_Presupuestaria", "Inciso" = "Inciso") == input$plot_group_by)),
        y = "Ejecutado (Millones de $)"
      ) +
      scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "tooltip_text") %>% config(displayModeBar = FALSE)
  })
}

shinyApp(ui = ui, server = server)
