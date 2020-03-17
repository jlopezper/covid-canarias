library(shiny)
library(shinydashboard)
source('./R/main.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$style("#Text{color: black;
                              font-size: 24px;,}
                       #Updtext{color: black;
                              font-size: 9px;}"
  )
  ),
  
  dashboardPage(
    # header
    header = dashboardHeader(title = 'Evolución COVID-19 en Canarias', titleWidth = 400),
    
    # sidebar
    dashboardSidebar(disable = TRUE),
    
    # body
    body = dashboardBody(
      fluidRow(column(width = 6, textOutput('Text')),
               column(width = 3, valueBoxOutput("TotalCases",width = 13)),
               column(width = 3, valueBoxOutput("Deaths",width = 13))),
      #fluidRow(valueBoxOutput("TotalCases", width = 3), valueBoxOutput("Deaths", width = 3)),
      fluidRow(column(width = 12, textOutput('Updtext'))),
      fluidRow(
        column(width = 6, 
               box(title = paste0("Casos activos a día: ", max(format(data_covid$fecha, '%d/%m/%Y'))), 
                   status = "primary",
                   width = 16.5, solidHeader = TRUE,
                   leafletOutput("mapPlot", height = 800))),
        column(width = 6,
               box(title = "Evolución diaria", 
                   status = "primary",
                   width = 16.5, solidHeader = TRUE,
                   plotly::plotlyOutput("EvolutionPlot", height = 800)))
      )
    )
  )
)


