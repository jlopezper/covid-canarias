library(shiny)
library(shinydashboard)
source("global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$style("#Text{color: black;
                              font-size: 24px;,}
                       #Updtext{color: black;
                              font-size: 9px;}")),

  dashboardPage(
    # header
    header = dashboardHeader(title = "Evolución COVID-19 en Canarias", titleWidth = 400),

    # sidebar
    dashboardSidebar(disable = TRUE),

    # body
    body = dashboardBody(
      fluidRow(
        column(width = 6, textOutput("Text")),
        column(width = 2, valueBoxOutput("Cases", width = 13)),
        column(width = 2, valueBoxOutput("ActiveCases", width = 13)),
        column(width = 2, valueBoxOutput("Deaths", width = 13))
      ),
      # fluidRow(valueBoxOutput("TotalCases", width = 3), valueBoxOutput("Deaths", width = 3)),
      fluidRow(column(width = 12, textOutput("Updtext"))),
      fluidRow(
        column(
          width = 6,
          box(
            title = paste0("Casos acumulados por 100.000 habitantes"),
            status = "primary",
            width = 16.5, solidHeader = TRUE,
            leafletOutput("mapPlot", height = 470)
          )
        ),
        column(
          width = 6,
          box(
            title = "Evolución diaria",
            status = "primary",
            width = 16.5, solidHeader = TRUE,
            plotly::plotlyOutput("EvolutionPlot", height = 470)
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            title = "Casos en Unidad de Cuidados Intensivos (UCI)",
            status = "primary",
            width = 16.5, solidHeader = TRUE,
            plotly::plotlyOutput("UciPlot", height = 470)
          )
        ),
        column(
          width = 6,
          box(
            title = "Incidencia acumulada/100.000 habitantes por grupo de edad y sexo",
            status = "primary",
            width = 16.5, solidHeader = TRUE,
            plotly::plotlyOutput("AgePlot", height = 470)
          )
        )
        # column(width = 6,
        #       box(title = "Fallecimientos por isla",
        #           status = "primary",
        #           width = 16.5, solidHeader = TRUE,
        #           plotly::plotlyOutput("DeathsPlot", height = 470)))
      ),
      fluidRow(
        column(
          width = 6,
          offset = 3,
          box(
            title = "Ajuste (curva logística, 200 simulaciones) y predicción de casos de UCI (3 días)",
            status = "primary",
            width = 16.5, solidHeader = TRUE,
            plotly::plotlyOutput("ModelPlot", height = 470)
          )
        )
      ),
      fluidRow(column(width = 12, textOutput("FootNote")))
    )
  )
)
