###----------EVALUACION SHINY APP----------
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyjs)
library(highcharter)
library(ggplot2)
library(DT)
library(dplyr)
library(scales)
library(bslib)
library(thematic)

###----------INTRODUCCION----------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(jsonlite)) install.packages("jsonlite")
library(tidyverse)
library(jsonlite)

obtener_indicadores <- function(empresa = "FALABELLA") {
  
  url <- stringr::str_c("https://www.elmercurio.com/inversiones/json/json.aspx?categoria=", empresa, "&time=10&indicador=2")
  
  df <- jsonlite::read_json(url)$Data %>%
    stringr::str_split(";") %>%
    dplyr::first() %>%
    I() %>%
    readr::read_delim(delim = ",", col_names = c("fecha", "Precio", "Volumen"))
  
  df <- df %>%
    mutate(
      Fecha = lubridate::ymd_hms(fecha),
      Anio = lubridate::year(fecha),
      Mes = lubridate::month(fecha)
    )
  
  df
} 

d <- obtener_indicadores("FALABELLA")
glimpse(d)

d %>%
  group_by(Anio) %>%
  summarise(mean(Precio))

ggplot(d) + 
  geom_line(aes(Fecha, Precio))

lista_empresas <- c("NUEVAPOLAR", "SMU", "BESALCO", "COPEC", "FALABELLA",
                    "BSANTANDER", "CMPC", "CHILE", "SQM-B", "ENELAM", "CENCOSUD",
                    "BCI", "LTM", "ENELCHILE", "SM-CHILE B", "CCU", "PARAUCO",
                    "ITAUCORP", "AGUAS-A", "COLBUN", "ENTEL", "ECL", "CONCHATORO",
                    "RIPLEY", "AESGENER", "ANDINA-B", "SONDA", "CAP", "ILC",
                    "SALFACORP", "SECURITY", "VAPORES", "ENELGXCH", "ANTARCHILE", 
                    "BANMEDICA", "EMBONOR-B", "FORUS", "IAM", "MASISA", "ORO BLANCO",
                    "SK", "SMSAAM")

###----------APP----------

ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "superhero"),
  
  titlePanel("Análisis de Inversiones en Chile"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("sliderfecha", 
                  h6("Seleccione Año"),
                  min = 2000, 
                  max = 2021, 
                  value = 2010,
                  ticks = TRUE,
                  sep = ""),
      conditionalPanel("input.sidebar === ’menu2’",
                       selectizeInput("select_tipo1",
                                      h6("Seleccione Empresa"),
                                      choices = lista_empresas,
                                      selected = "", width = "300px",
                                      multiple = F))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(h6("Precio"),
                 h5("Gráfico de Inversiones"),
                 plotOutput("grafprecio"),
                 h5("Tabla de Inversiones"),
                 fluidRow(dataTableOutput("tablaprecio")),
                 ),
        tabPanel(h6("Volúmen"),
                 h5("Gráfico de Inversiones"),
                 plotOutput("grafvolumen"),
                 h5("Tabla de Inversiones"),
                 fluidRow(dataTableOutput("tablavolumen")))
      )
    )
  )
)

server <- function(input, output) {
  output$grafprecio <- renderPlot(
    ggplot(obtener_indicadores(input$select_tipo1) %>% 
             filter(Anio == input$sliderfecha),
           aes(x = as.Date(Fecha), y = Precio)) +
      labs(title = paste0("Inversiones de ", input$select_tipo1),
           x = "Fecha", y = "Precio") +
      geom_point(color = "darkblue") +
      geom_line(color = "darkblue") +
      scale_y_continuous(labels = dollar) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
      theme_minimal()
  )
  
  output$grafvolumen <- renderPlot(
    ggplot(obtener_indicadores(input$select_tipo1) %>%
             filter(Anio == input$sliderfecha),
           aes(x = as.Date(Fecha), y = Volumen/1000000)) +
      labs(title = paste0("Inversiones de ", input$select_tipo1),
           x = "Fecha", y = "Volúmen (en millones)") +
      geom_point(color = "darkblue") +
      geom_line(color = "darkblue") +
      scale_y_continuous(labels = comma) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
      theme_minimal()
  )
  
  output$tablaprecio <- renderDataTable({
    datatable(obtener_indicadores(input$select_tipo1)[,c(-1, -3, -4)], 
              #%>% filter(Anio == input$sliderfecha),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center;',
                htmltools::em(paste0("Tabla resumen de ", input$select_tipo1))),
              colnames = c("Precio", "Año", "Mes"),
              filter = "top",
              options = list(
                             pageLength = 10,
                             autoWidth = TRUE)
                             #dom = "t")
                             #scrollX = TRUE,
                             #scrollY = TRUE)
              )
  })
  
  output$tablavolumen <- renderDataTable({
    datatable(obtener_indicadores(input$select_tipo1)[,c(-1, -2, -4)],
              #%>% filter(Anio == input$sliderfecha),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center;',
                htmltools::em(paste0("Tabla resumen de ", input$select_tipo1))),
              colnames = c("Volúmen", "Año", "Mes"),
              filter = "top", 
              options = list(
                            pageLength = 10,
                            autoWidth = TRUE,
                            dom = "t")
                            #scrollX = TRUE,
                            #scrollY = TRUE)
              )
  })
}

shinyApp(ui, server)
