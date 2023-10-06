#install.packages("clipr")
library(shiny)
library(DT)
library(highcharter)
library(tidyr)
library(tidyverse)
library(glue)
library(shinyjs)
library(clipr)
library(bslib)
library(shinythemes)


# datos1=read.csv("Base_Arbo_Brote-2023-10-03.csv")
# 
# datos = datos1 %>%  filter(
#   EVENTO == "Dengue" &
#     MIN_ANIO == 2023 &
#     (CLASI_RESUMEN == "Confirmado" |
#        CLASI_RESUMEN == "Probable")
# ) %>% select(
#   IDEVENTOCASO,
#   PROVINCIA_RESIDENCIA,
#   PROVINCIA_CARGA,
#   PROV_RESI_CARGA,
#   SEXO,
#   EDAD_APERTURA,
#   GRUPO_ETARIO,
#   FECHA_APERTURA,
#   SEPI_APERTURA,
#   MIN_FECHA,
#   MIN_ANIO,
#   MIN_SE,
#   Arbo_pos,
#   CLASI_RESUMEN,
#   antecedente_viaje_final_ok
# )
#
# write.csv(datos,"datosapp.csv")

datos = read.csv("~/curso_shiny_vilsi/TF/datosapp.csv")

casos = datos %>% group_by(PROV_RESI_CARGA) %>% summarise(
  Autóctono = sum(antecedente_viaje_final_ok == "Autóctono", na.rm = TRUE),
  En_investigación = sum(antecedente_viaje_final_ok == "En Investigación", na.rm = TRUE),
  Importado = sum(antecedente_viaje_final_ok == "Importado", na.rm = TRUE),
  'Total casos dengue'=n()
)

total = datos %>% summarise(
  Autóctono = sum(antecedente_viaje_final_ok == "Autóctono", na.rm = TRUE),
  En_investigación = sum(antecedente_viaje_final_ok == "En Investigación", na.rm = TRUE),
  Importado = sum(antecedente_viaje_final_ok == "Importado", na.rm = TRUE),
  'Total casos dengue'=n()
)

casostotal = bind_rows(casos,total)

casostotal[(dim(casostotal)[1]), 1] = "Total país"

casostotal = casostotal %>% rename('Provincia de residencia' = PROV_RESI_CARGA,
                                   'En investigación' = En_investigación)

casos_se = datos %>% group_by(PROV_RESI_CARGA, MIN_SE) %>% summarise(Casos=n())

casos_se_total = datos %>% group_by(MIN_SE) %>% summarise(Casos=n())
casos_se_total$PROV_RESI_CARGA = "Total país"

casos_se = bind_rows(casos_se,casos_se_total)


ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  fluidRow(
    column(12,
           h1(strong("Situación de Dengue en Argentina. Año 2023")),
           align= "center" 
    )
  ),
  hr(),
  fluidRow(
    column(width = 3,
           
           h2("Casos por jurisdicción"),
           br(),
           selectInput(
             inputId = "selectProvincia",
             label = "Seleccionar jurisdicciones:",
             choices = casostotal$`Provincia de residencia`,
             selected = "Total país",
             multiple = TRUE
           ),
           br(),
           actionButton(
               inputId = "mostrar_grafico",
               label = "Mostrar gráfico"
             ),
           br(),
           disabled(actionButton(
             inputId = "limpiar",
             label = "Limpiar"
           )),
           br(),
           br(),
           br(),
           br(),
           h2("Tabla según antecedente Epi"),
           br(),
           actionButton(
             "mostrar_tabla",
             "Mostrar tabla"
           ),
           br(),
           br(),
           hidden(
             downloadButton(
               "descargar",
               "Descargar tabla"
             )),
           align="center"),
    
    column(width = 9,
           #h2("Casos por semana epi"),
           highcharter::highchartOutput("grafico"),
           br(),
           DT::DTOutput("tabla"),
          
           align="center")
  )
)

server <- function(input, output, session) {
  #para modificar tema pegar; bslib::bs_themer()
  
  session$onSessionEnded(function(){
    stopApp()
  })
  
  observeEvent(input$mostrar_grafico, {
    show("grafico")
    output$grafico= renderHighchart({
      jurisdiccion=input$selectProvincia
      
      niveles_prov <- unique(jurisdiccion)
      
      niveles_sem <- seq(0,max(casos_se$MIN_SE),by=1)
      
      # armo el grafico con highchart
      hc <- highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "Casos de dengue según semana epidemiológica") %>%
        hc_xAxis(title = list(text = "Semana Epidemiológica"), categories = niveles_sem) %>%
        hc_yAxis(title = list(text = "Casos")) %>%
        hc_caption(text = "</b><br><em>Fuente: Elaboración propia a partir de datos del SNVS2.0</em>") %>% 
        hc_exporting(enabled = TRUE) # enable exporting option
      
      # Agrega una serie de datos para cada nivel de "prov"
      for (nivel in niveles_prov) {
        data_serie <- casos_se[casos_se$PROV_RESI_CARGA == nivel,]
        hc <- hc %>%
          hc_add_series(
            data_serie,
            "column",
            hcaes(x = MIN_SE, y = Casos),
            name = nivel,
            marker = list(radius = 4)
          )
      }
      print(hc)
    })
    disable("mostrar_grafico")
    disable("selectProvincia")
    enable("limpiar")
  })
  
  observeEvent(input$limpiar, {
    hide("grafico")
    enable("selectProvincia")
    enable("mostrar_grafico")
    disable("limpiar")
    hide("tabla")
    enable("mostrar_tabla")
  })
  
  
  observeEvent(input$mostrar_tabla, {
    
    show("tabla")
    output$tabla = DT::renderDataTable(casostotal,caption = "Casos de dengue según antecedente epidemiológico. Argentina, SE 1 a SE 39 de 2023")
    
    show("descargar", anim = T, animType = "fade")
    disable("mostrar_tabla")
  })
  
  output$descargar = downloadHandler(
    
    filename = function(){
      paste('Casos dengue Antecedente Epi. ',Sys.Date(),'.csv', sep = '')
    },
    content = function(file){
      datos_descarga = casostotal
      
      write.csv(datos_descarga, file, row.names = F)
    }
    
  )
  
   
}

shinyApp(ui, server)
