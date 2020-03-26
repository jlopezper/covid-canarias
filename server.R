library(here)

intro <- "Este dashboard presenta los resultados diarios relativos al COVID-19 publicados \
por la Consejería de Sanidad del Gobierno de Canarias.*"
footnote <- "* Nota: los datos pueden tener ligeras discrepancias entre gráficos \
debido a la precariedad en la toma de la información. Parte de los valores provienen \
de notas de prensa de la Consejería y otros del informe epidemiológico que publica la misma \
diariamente."

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$Text <- renderText({
    intro
  })
  output$Updtext <- renderText({
    paste0("Datos actualizados a: ", format(max(data_covid$fecha), "%d/%m/%Y"))
  })
  output$FootNote <- renderText({
    footnote
  })


  output$ActiveCases <- renderValueBox({
    valueBox(
      subtitle = "Total casos activos",
      value = key_values[[1]]$X[[3]],
      icon = icon("exclamation-triangle"),
      color = "orange"
    )
  })


  output$Cases <- renderValueBox({
    valueBox(
      subtitle = "Total casos",
      value = filter(data_covid_filt, isla == "Total")$casos,
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })

  output$Deaths <- renderValueBox({
    valueBox(
      subtitle = "Fallecimientos",
      value = key_values[[1]]$X.1[[5]],
      icon = icon("exclamation-triangle"),
      color = "red"
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
        pal = pal, values = ~casos, opacity = 0.7, title = "Número de casos acumulados/100.000 hab",
        position = "bottomright"
      )
  })


  output$EvolutionPlot <- renderPlotly({
    dd <- data_covid %>%
      select(fecha, isla, casos) %>%
      pivot_wider(names_from = isla, values_from = casos)

    # dd$Total <- rowSums(select(dd, -c(fecha)))

    fig <- plot_ly(dd, x = ~fecha, y = ~`Gran Canaria`, name = "Gran Canaria", type = "scatter", mode = "lines")
    fig <- fig %>% add_trace(y = ~`Tenerife`, name = "Tenerife", mode = "lines")
    fig <- fig %>% add_trace(y = ~`La Gomera`, name = "La Gomera", mode = "lines")
    fig <- fig %>% add_trace(y = ~`El Hierro`, name = "El Hierro", mode = "lines")
    fig <- fig %>% add_trace(y = ~`La Palma`, name = "La Palma", mode = "lines")
    fig <- fig %>% add_trace(y = ~`Lanzarote`, name = "Lanzarote", mode = "lines")
    fig <- fig %>% add_trace(y = ~`Fuerteventura`, name = "Fuerteventura", mode = "lines")
    fig <- fig %>% add_trace(y = ~`Total`, name = "Total", mode = "lines")
    fig %>% layout(
      hovermode = "compare",
      xaxis = list(
        title = "Fecha",
        type = "date",
        tickformat = "%d-%m-%Y"
      ),
      yaxis = list(title = "Casos acumulados"),
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
      data_uci %>%
      mutate(fall_crec = casos_uci / lag(casos_uci) - 1) %>%
      filter(!is.na(fall_crec), casos_uci >= 10)


    ddd <- left_join(data_uci, data_deaths, by = "fecha") %>%
      left_join(dd[c("fecha", "fall_crec")], by = "fecha")

    plot_ly(ddd, x = ~fecha) %>%
      add_trace(y = ~casos_uci, name = "Casos en UCI", type = "scatter", mode = "lines", line = list(color = "#395C6B")) %>%
      add_trace(y = ~fallecimientos, name = "Fallecimientos", type = "scatter", mode = "lines", line = list(color = "80A4ED")) %>%
      add_trace(
        x = ~fecha, y = ~fall_crec, type = "bar", name = "Cambio relativo casos en UCI (> 9 casos)", yaxis = "y2",
        marker = list(color = "#bfbfbf"),
        hoverinfo = "text",
        text = ~ paste("Crecimiento relativo: ", round(fall_crec * 100, 2), "%")
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
        yaxis2 = list(side = "right", title = "Crecimiento relativo de casos en UCI", showgrid = FALSE, zeroline = FALSE, tickformat = "%", rangemode = "tozero", automargin = TRUE),
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
      rename(Edad = Grupo.de.Edad) %>%
      select(Edad, Mujer.1, Hombre.1) %>%
      rename(
        Hombre = Hombre.1,
        Mujer = Mujer.1
      )

    df$Hombre <- as.numeric(gsub(pattern = ",", replacement = ".", x = df$Hombre))
    df$Mujer <- as.numeric(gsub(pattern = ",", replacement = ".", x = df$Mujer))



    df$Edad <- factor(df$Edad, levels = c("10-19 años", "20-29 años", "30-39 años", "40-49 años", "50-59 años", "60-69 años", "70-79 años", "80-89 años", ">=90 años"), ordered = TRUE)

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
      tooltip = c("casos", "prediccion_mediana")
    )
  })
}
