library(here)

intro <- "Este dashboard presenta los resultados diarios relativos al COVID-19 publicados \
por la Consejería de Sanidad del Gobierno de Canarias y a través de la información recopilada por Datadista.*"
footnote <- "* Nota: los datos pueden tener ligeras discrepancias entre gráficos \
debido a la precariedad información proveniente de la Consejería. Parte de los valores provienen \
de notas de prensa y otros del informe epidemiológico que publica la misma \
diariamente. Gracias a Datadista por publicar información diaria actualizada por CCAA."

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # output$Text <- renderText({
  #   intro
  # })
  # output$Updtext <- renderText({
  #   paste0("Datos actualizados a: ", format(max(data_covid$fecha), "%d/%m/%Y"))
  # })
  output$FootNote <- renderText({
    footnote
  })


  output$ActiveCases <- renderValueBox({
    
    a1 <- as.numeric(gsub(pattern = "\\.", 
                          replacement = "", 
                          x = key_values[[1]]$Activos[[length(key_values[[1]]$Activos)]]))
    
    a2 <- as.numeric(gsub(pattern = "\\.", 
                          replacement = "", 
                          x = key_values[[1]]$X.1[[length(key_values[[1]]$X.1)]]))
    
    a3 <- as.numeric(gsub(pattern = "\\.", 
                          replacement = "", 
                          x = key_values[[1]]$X.2[[length(key_values[[1]]$X.2)]]))
    
    
    valueBox(
      subtitle = "Total casos activos",
      value = a1 + a2 + a3,
      icon = icon("exclamation-triangle"),
      color = "orange"
    )
  })


  output$Cases <- renderValueBox({
    valueBox(
      subtitle = "Total casos",
      value = as.numeric(gsub(pattern = "\\.", 
                              replacement = "", 
                              x = key_values[[1]]$Suma.Total[[length(key_values[[1]]$Suma.Total)]])),
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })

  output$Deaths <- renderValueBox({
    valueBox(
      subtitle = "Fallecimientos",
      value = as.numeric(gsub(pattern = "\\.", 
                              replacement = "", 
                              x = key_values[[1]]$Cerrados[[length(key_values[[1]]$Cerrados)]])),
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  
  output$Recovered <- renderValueBox({
      valueBox(
          subtitle = "Altas",
          value = filter(data_altas, fecha == max(fecha))$altas,
          icon = icon("exclamation-triangle"),
          color = "green"
      )
  })
  
  output$UCI <- renderValueBox({
      valueBox(
          subtitle = "UCI",
          value = filter(data_uci, fecha == max(fecha))$casos_uci,
          icon = icon("exclamation-triangle"),
          color = "blue"
      )
  })
  
  
  output$Hospitalizados <- renderValueBox({
      valueBox(
          subtitle = "Hospitalizados",
          value = filter(data_hospitalizados, fecha == max(fecha))$hospitalizados,
          icon = icon("exclamation-triangle"),
          color = "light-blue"
      )
  })

  output$mapPlot <- renderLeaflet({
    poblacion <- tibble(
      isla = c("Fuerteventura", "Gran Canaria", "Lanzarote", "La Gomera", "El Hierro", "La Palma", "Tenerife"),
      pob = c(116886, 851231, 152289, 21503, 10968, 82671, 917841)
    )

    dd <-
      canarias %>%
      left_join(poblacion, by = "isla") %>%
      mutate(
        casos = casos / pob * 100000,
        pob = NULL
      )

    pal <- colorNumeric(
      palette = "Reds",
      domain = dd$casos
    )

    labels <- sprintf(
      "<strong>%s</strong><br/>%g casos acumulados/100.000 hab",
      dd$isla, round(dd$casos, 2)
    ) %>%
      lapply(htmltools::HTML)


    leaflet(dd) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(
        fillColor = ~ pal(casos),
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
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, values = ~casos, opacity = 0.7, title = "Casos acumulados/100.000 hab",
        position = "bottomright"
      )
  })


  output$EvolutionPlot <- renderPlotly({
    dd <- data_covid %>%
      select(fecha, isla, casos) %>%
      pivot_wider(names_from = isla, values_from = casos)

    # dd$Total <- rowSums(select(dd, -c(fecha)))
    
    vline <- function(x = 0, color = "red") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color, dash = "dot")
      )
    }
    
    fig <- plot_ly(dd, x = ~fecha, y = ~`Gran Canaria`, name = "Gran Canaria", type = "scatter", mode = "lines")
    fig <- fig %>% add_trace(y = ~`Tenerife`, name = "Tenerife", mode = "lines")
    fig <- fig %>% add_trace(y = ~`La Gomera`, name = "La Gomera", mode = "lines")
    fig <- fig %>% add_trace(y = ~`El Hierro`, name = "El Hierro", mode = "lines")
    fig <- fig %>% add_trace(y = ~`La Palma`, name = "La Palma", mode = "lines")
    fig <- fig %>% add_trace(y = ~`Lanzarote`, name = "Lanzarote", mode = "lines")
    fig <- fig %>% add_trace(y = ~`Fuerteventura`, name = "Fuerteventura", mode = "lines")
    fig <- fig %>% add_trace(y = ~`Total`, name = "Total", mode = "lines")
    fig <- fig %>% add_annotations(
      x = as.Date('2020-04-06'),
      y = 670,
      xref = "x",
      yref = "y",
      text = "Cambio criterio Sanidad",
      showarrow = T,
      ax = -75,
      ay = 20,
      font = list(size = 9)
    )
    
    fig %>% layout(
      hovermode = "compare",
      xaxis = list(
        title = "Fecha",
        type = "date",
        tickformat = "%d-%m-%Y"
      ),
      yaxis = list(title = "Casos acumulados"),
      shapes = list(vline(as.Date('2020-04-06'))),
      updatemenus = list(
        list(
          active = 0,
          buttons = list(
            list(
              label = "Lineal",
              method = "update",
              args = list(list(visible = c(TRUE, TRUE)), list(yaxis = list(
                type = "linear",
                title = "Casos acumulados"
              )))
            ),
            list(
              label = "Log",
              method = "update",
              args = list(list(visible = c(TRUE, TRUE)), list(yaxis = list(
                type = "log",
                title = "Casos acumulados (log scale)"
              )))
            )
          )
        )
      )
    )
  })


  output$UciPlot <- renderPlotly({
    dd <-
      data_deaths %>%
      mutate(fall_crec = fallecimientos / lag(fallecimientos) - 1) %>%
      filter(!is.na(fall_crec), fallecimientos >= 10)


    ddd <- left_join(data_uci, data_deaths, by = "fecha") %>%
      left_join(dd[c("fecha", "fall_crec")], by = "fecha")

    plot_ly(ddd, x = ~fecha) %>%
      add_trace(y = ~casos_uci, name = "Casos en UCI", type = "scatter", mode = "lines", line = list(color = "#395C6B")) %>%
      add_trace(y = ~fallecimientos, name = "Fallecimientos", type = "scatter", mode = "lines", line = list(color = "80A4ED")) %>%
      add_trace(
        x = ~fecha, y = ~fall_crec, type = "bar", name = "Tasa variación fallecimientos (> 9 fallecidos)", yaxis = "y2",
        marker = list(color = "#bfbfbf"),
        hoverinfo = "text",
        text = ~ paste("Cambio relativo: ", round(fall_crec * 100, 2), "%")
      ) %>%
      layout(
        hovermode = "compare",
        xaxis = list(
          title = "Fecha",
          type = "date",
          tickformat = "%d-%m-%Y"
        ),
        xaxis = list(automargin = TRUE),
        yaxis = list(side = "left", overlaying = "y2", title = "Casos acumulados (total Canarias)", showgrid = FALSE, zeroline = FALSE, rangemode = "tozero"),
        yaxis2 = list(side = "right", title = "Tasa variación fallecimientos", showgrid = FALSE, zeroline = FALSE, tickformat = "%", rangemode = "tozero", automargin = TRUE, range = c(0,1)),
        annotations = list(
          x = 1, y = -0.1, text = "Fuente: Datadista",
          showarrow = F, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = 1
        )
      )
  })


  output$DeathsPlot <- renderPlotly({
    dd <-
      data_covid %>%
      count(isla, wt = fallecimientos, name = "fallecidos")

    dd$isla <- factor(dd$isla, levels = unique(dd$isla)[order(dd$fallecidos, decreasing = TRUE)])

    fig <- plot_ly(
      data = dd,
      x = ~isla,
      y = ~fallecidos,
      text = ~fallecidos,
      textposition = "auto",
      type = "bar",
      marker = list(color = "#395C6B")
    )

    fig
  })



  output$AgePlot <- renderPlotly({
    df <- data.frame(pdf_lista[[1]])
    df <-
      df %>%
      rename(Edad = Grupo.de.edad) %>%
      select(Edad, Mujer, Hombre)
    
    df$Hombre <- as.numeric(gsub(pattern = ",", replacement = ".", x = df$Hombre))
    df$Mujer <- as.numeric(gsub(pattern = ",", replacement = ".", x = df$Mujer))



    df$Edad <- factor(df$Edad, levels = c("10-19 años", "20-29 años", "30-39 años", "40-49 años", "50-59 años", "60-69 años", "70-79 años", "80-89 años", "90 o más años"), ordered = TRUE)

    fig <- plot_ly(df, x = ~Edad, y = ~Hombre, type = "bar", name = "Hombre", marker = list(color = "#395C6B"))
    fig <- fig %>% add_trace(y = ~Mujer, name = "Mujer", marker = list(color = "#80A4ED"))
    fig <- fig %>% layout(yaxis = list(title = "Count"), barmode = "group")
    fig %>%
      layout(
        hovermode = "compare",
        yaxis = list(title = "Incidencia acumulada/100k hab."),
        annotations = list(
          x = 0.2, y = 1.05, text = "Fuente: Informe epidemiológico",
          showarrow = F, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        )
      )
  })


  output$ModelPlot <- renderPlotly({
    preds_boot <- readRDS(here("data", "preds_boot.rds"))
    new_preds <- readRDS(here("data", "new_preds.rds"))
    preds <- readRDS(here("data", "preds.rds"))
    new_pred <-
      readRDS(here("data", "new_pred.rds")) %>%
      rename(prediccion_mediana = .fitted_med)
    data_model <- readRDS(here("data", "data_model.rds"))



    ggplotly(
      bind_rows(preds_boot, new_preds) %>%
        ggplot(aes(fecha, .fitted)) +
        geom_line(aes(group = boot_num), alpha = 0.01) +
        geom_line(aes(fecha, .fitted, color = "Ajuste logístico"), preds) +
        geom_point(aes(fecha, casos, color = "Casos UCI"), data_model) +
        geom_line(aes(fecha, casos, color = "Casos UCI"), data_model) +
        geom_point(aes(fecha, prediccion_mediana, color = "Predicción mediana (I.C. 90%)"), new_pred) +
        # geom_line(aes(fecha, .fitted_med, color = 'Predicción'), new_pred) +
        geom_errorbar(aes(fecha, ymin = lwr_CI, ymax = upr_CI), data = new_pred, color = "red", alpha = 0.4) +
        # geom_ribbon(aes(fecha, ymin = lwr_CI, ymax = upr_CI), fill = 'red', data = new_pred, alpha = .1) +
        scale_color_manual(values = c("Ajuste logístico" = "black", "Casos UCI" = "#395C6B", "Predicción mediana (I.C. 90%)" = "red")) +
        theme_minimal(base_family = "Arial") +
        labs(
          x = "Fecha",
          y = "Casos",
          color = ""
        ),
      tooltip = c("casos", "prediccion_mediana", "x")
    )
  })
  
  
  
  output$SplitPlot <- renderPlotly({
      dd <-
          list(data_deaths, data_uci, data_altas, data_hospitalizados) %>% 
          reduce(left_join, by = "fecha") %>% 
          filter(!is.na(hospitalizados))
      
      fig <- plot_ly(dd, x = ~fecha, y = ~fallecimientos, type = 'bar', name = 'Fallecimientos', marker = list(color = '#5B5B5B'))
      fig <- fig %>% add_trace(y = ~casos_uci, name = 'UCI', marker = list(color = '#2F4550'))
      fig <- fig %>% add_trace(y = ~altas, name = 'Altas', marker = list(color = '#B8DBD9'))
      fig <- fig %>% add_trace(y = ~hospitalizados, name = 'Hospitalizados', marker = list(color = '#617F91'))
      fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
      
      fig %>% 
          layout(
              hovermode = "compare",
              yaxis = list(title = "Casos"),
              annotations = list(
                  x = 1.1, y = -0.1, text = "Fuente: Datadista",
                  showarrow = F, xref = "paper", yref = "paper",
                  xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                  font = list(size = 10, color = "black")
              )
          )
          
      
  })
}
