#install.packages("clipr")
library(shiny)
library(DT)
library(highcharter)
library(tidyr)
library(tidyverse)
library(glue)
library(shinyjs)
library(clipr)

datos=iris

ui <- fluidPage(
  useShinyjs(),
  fluidRow(
    column(12,
           h1(strong("Título de la aplicación")),
           align= "center" 
    )
  ),
  hr(),
  fluidRow(
    column(width = 3,
           
           h2("Filtros"),
           selectInput(
             inputId = "selectSpecie",
             label = "Seleccionar especie:",
             choices = unique(datos$Species),
             selected = "setosa"
           ),
           actionButton(
             "boton",
             "Ver filas"
           ),
           actionButton(
             "clip",
             "Copiar datos"
           ),
           checkboxInput(
             inputId = "mostrar_boxplot",
             label = "Mostrar / Ocultar Boxplot",
             value = TRUE
           ),
           
           align="center"),
    column(width = 9,
           h2("Tabla"),
           DT::DTOutput("tabla"),
           br(),
           highcharter::highchartOutput("boxplot"),
           align="center")
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$mostrar_boxplot, {
    if (input$mostrar_boxplot==T) {
      show("boxplot")
    } else {
      hide("boxplot")
    }
  })
  
  datosProcesados =reactive({
    especieSeleccionada = input$selectSpecie
    datos =  datos[datos$Species==especieSeleccionada,]
    datos
  })
  
  output$tabla = DT::renderDataTable({
    print(input$selectSpecie)
    especieSeleccionada = input$selectSpecie
    datos = datosProcesados()
    
    DT::datatable(datos, caption = paste0("Especie seleccionada: ",especieSeleccionada))
    
  })
  output$boxplot = renderHighchart({
    datos = datos %>% pivot_longer(cols = 1:4, values_to = "Valores", names_to = "Medidas")
    datos$Species = NULL
    
    datos$Medidas = factor(datos$Medidas)
    datos = datos %>% as_tibble()
    
    # grafico
    data_boxplot=data_to_boxplot(
      data = datos,
      variable = Valores,
      group_var = Medidas,
      group_var2 = Medidas,
      add_outliers = F,
      fillColor = c('#a6611a','#dfc27d','#80cdc1','#018571'),
      color="black"
    )
    
    highchart()%>%
      hc_xAxis(type ="category")%>%
      hc_add_series_list(data_boxplot)%>%
      hc_xAxis(title = list(text = "Medida"))%>%
      hc_yAxis(title = list(text = "Centímetros"))%>%
      hc_title(text= glue("Boxplot para medidas de <em>{input$selectSpecie}</em>")) %>%
      hc_subtitle(text= "Medidas de pétalo y sépalo") %>%
      hc_legend(enabled= FALSE)
    
    
  })
  
}

shinyApp(ui, server)
