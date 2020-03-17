library(shinydashboard)
library(leaflet)
library(leafpop)
library(leafletCN)
library(plotly)
source('./R/main.R')

intro <- "Este dashboard presenta los resultados diarios relativos al COVID-19 publicados \
por la Consejería de Sanidad del Gobierno de Canarias."

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$Text <- renderText({intro})
    output$Updtext <- renderText({paste0('Datos actualizados a: ', format(max(data_covid$fecha), '%d/%m/%Y'))})
    
    
    output$TotalCases <- renderValueBox({
        valueBox(
            subtitle = "Total casos activos",
            value =  sum(data_covid_filt$casos_activos),
            icon = icon("exclamation-triangle"),
            color = "orange"
        )
    })
    
    output$Deaths <- renderValueBox({
        valueBox(
            subtitle = "Fallecimientos",
            value =  sum(data_covid$fallecimientos),
            icon = icon("exclamation-triangle"),
            color = "red"
        )
    })
    
    output$mapPlot <- renderLeaflet({
        
        leaflet(canarias) %>%
            addTiles() %>% 
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
            addPolygons(
                fillColor = ~pal(casos_activos),
                weight = 0.1,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 1,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
            addLegend(pal = pal, values = ~casos_activos, opacity = 0.7, title = 'Número de casos',
                      position = "bottomright") 
        
    })
    
    
    output$EvolutionPlot <- renderPlotly({
        #data_covid %>% 
        #    select(fecha, isla, casos_activos) %>% 
        #    pivot_wider(names_from = isla, values_from = casos_activos)
        #ggplot(aes(x = fecha, y = casos_activos)) +
        #    geom_line(aes(color = isla)) +
        #    geom_point(aes(color = isla)) +
        #    # scale_y_log10() +
        #    theme_minimal() +
        #    theme(plot.title = element_text(size=22),
        #          legend.text=element_text(size = 15),
        #          axis.text=element_text(size = 15),
        #          axis.title=element_text(size=15)) +
        #    scale_color_brewer(palette="Dark2") +
        #    labs(title = '',
        #         x = 'Fecha',
        #         y = 'Casos activos', 
        #         color = "") 
        
        dd <- data_covid %>% 
            select(fecha, isla, casos_activos) %>% 
            pivot_wider(names_from = isla, values_from = casos_activos) 
        
        dd$Total <- rowSums(select(dd, -c(fecha)))
        
        fig <- plot_ly(dd, x = ~fecha, y = ~`Gran Canaria`, name = 'Gran Canaria', type = 'scatter', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~`Tenerife`, name = 'Tenerife', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~`La Gomera`, name = 'La Gomera', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~`El Hierro`, name = 'El Hierro', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~`La Palma`, name = 'La Palma', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~`Lanzarote`, name = 'Lanzarote', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~`Fuerteventura`, name = 'Fuerteventura', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~`Total`, name = 'Total', mode = 'lines+markers')
        fig %>% layout(hovermode = 'compare',
                       xaxis = list(title = 'Fecha',
                           type = 'date',
                           tickformat = "%d-%m-%Y"
                       ),
                       yaxis = list(title = 'Casos activos'))
        
    }
    )
    
}

