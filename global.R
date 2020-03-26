# Cargar librerias
library(tidyverse)
library(geojsonsf)
library(sf)
library(leaflet)
library(tmap)
library(leafpop)
library(tabulizer)
library(xml2)
library(nlstools)
library(nls.multstart)
library(broom)
library(leafpop)
library(leafletCN)
library(plotly)
library(shiny)
library(shinydashboard)
library(tibble)
library(tidyr)
library(lubridate)
library(here)


# Utils
source("config.R")


# Crear los csv con los datos de casos acumulados
build_data <- function(url) {

  # crear directorio si no existe
  if (!dir.exists(here("data"))) {
    dir.create(here("data"))
  }

  # intentar leer informacion historica
  historico <- try(read_csv(here("data", "historico.csv")), silent = TRUE)

  # leer tabla de las notas de prensa diarias
  # este proceso es muy fragil porque cada dia es una loteria
  # no se sabe que informacion publicara la consejeria
  webpage <- xml2::read_html(url)
  raw_table <- rvest::html_table(webpage)[[1]]
  col_names <- unlist(raw_table[1, ], use.names = FALSE)
  names(raw_table) <- c("isla", col_names[-1])
  raw_table <-
    raw_table[-1, ] %>%
    as_tibble() %>%
    mutate(
      isla = ifelse(isla == "G.CANARIA", "GRAN CANARIA", isla),
      isla = stringi::stri_trans_totitle(isla),
    ) %>%
    pivot_longer(cols = -c("isla"), names_to = "fecha", values_to = "casos") %>%
    mutate(
      fecha = as_date(parse_date_time(x = .$fecha, orders = c("dm", "dmY", "dmY", "dmy"))),
      casos = as.integer(casos)
    )
  year(raw_table$fecha) <- year(Sys.Date())



  # si existe el historico, hacer un bind rows con la nueva informacion
  # quedandonos solo con los dias que no aparecen previamente
  if (!is(historico, "try-error")) {
    historico <-
      bind_rows(
        historico,
        raw_table[!raw_table$fecha %in% historico$fecha, ]
      ) %>%
      arrange(isla, fecha)

    write_csv(historico, here("data", "historico.csv"))
  } else {
    write_csv(raw_table, here("data", "historico.csv"))
  }
}


build_data(url_casos)

# Leer distintas fuentes de info
canarias <- geojson_sf(here("data", "ISLANDS.geojson"))
data_covid <- read_csv(here("data", "historico.csv"))
data_covid_filt <- filter(data_covid, fecha == max(fecha)) # solo con la ultima fecha
canarias_coord <- read_delim(here("data", "islas_coordenadas.csv"), delim = ";")
data_uci <- readr::read_csv(datos_uci)
data_deaths <- readr::read_csv(datos_fallecidos)
data_altas <- read_csv(datos_altas)
data_hospitalizados <- read_csv(datos_hospitalizados)


# Modificar los datos que provienen de datadista (UCI y fallecimientos)
# para darles un formato amigable
filter_can <- 
  function(df, var) {
    var <- enquo(var)
    
    df %>%
      select(-cod_ine) %>%
      filter(CCAA == "Canarias") %>%
      select(fecha,  !! var := total)
  }


data_uci <- filter_can(data_uci, casos_uci)
data_deaths <- filter_can(data_deaths, fallecimientos)
data_altas <- filter_can(data_altas, altas)
data_hospitalizados <- filter_can(data_hospitalizados, hospitalizados)


canarias <-
  canarias %>%
  left_join(data_covid_filt[c("isla", "casos")], by = c("label" = "isla")) %>%
  left_join(canarias_coord, by = c("label" = "isla")) %>%
  rename(isla = label)



# extraer informacion de los informes epidemiologicos
extract_from_pdf <- function(url, coords) {
  # 'https://www3.gobiernodecanarias.org/sanidad/scs/content/dcb400c5-6504-11ea-9a8e-719d4b52bf6c/InformeCasosCOVID-19.pdf'
  extract_tables(
    url,
    output = "data.frame",
    pages = c(1),
    area = list(
      coords
    ),
    guess = FALSE,
    encoding = "UTF-8"
  )
}

pdf_lista <- extract_from_pdf(
  url = "https://www3.gobiernodecanarias.org/sanidad/scs/content/dcb400c5-6504-11ea-9a8e-719d4b52bf6c/InformeCasosCOVID-19.pdf",
  coords = c(559.53149, 72.97314, 703.78571, 523.22114)
)

key_values <- extract_from_pdf(
  url = "https://www3.gobiernodecanarias.org/sanidad/scs/content/dcb400c5-6504-11ea-9a8e-719d4b52bf6c/InformeCasosCOVID-19.pdf",
  coords = c(349.7072, 162.1485, 458.1164, 432.2973)
)



# Modelo (solo se ejecuta offline)
build_model <- function() {
  # seleccionar solo informacion relevante
  mm <-
    data_uci %>%
    mutate(fecha_index = as.numeric(fecha - min(fecha))) %>%
    select(fecha_index, fecha, casos = casos_uci)

  fit <- nls_multstart(casos ~ SSlogis(fecha_index, phi1, phi2, phi3),
    data = data.frame(mm),
    iter = 100,
    start_lower = c(phi1 = 1000, phi2 = 10, phi3 = 0),
    start_upper = c(phi1 = 2000, phi2 = 20, phi3 = 5),
    lower = c(phi1 = 1000, phi2 = 10, phi3 = 0),
    supp_errors = "Y"
  )

  preds <- augment(fit)

  new_pred <- tibble(
    fecha_index = c(max(mm$fecha_index) + 1, max(mm$fecha_index) + 2, max(mm$fecha_index) + 3),
    fecha = c(max(mm$fecha) + 1, max(mm$fecha) + 2, max(mm$fecha) + 3),
    .fitted = predict(fit, data.frame(fecha_index = c(max(mm$fecha_index) + 1, max(mm$fecha_index) + 2, max(mm$fecha_index) + 3)))[1:3]
  )

  fit_boots <- mm %>%
    modelr::bootstrap(n = 200, id = "boot_num") %>%
    group_by(boot_num) %>%
    mutate(fit = map(strap, ~ nls_multstart(casos ~ SSlogis(fecha_index, phi1, phi2, phi3),
      data = data.frame(.),
      iter = 100,
      start_lower = c(phi1 = 1000, phi2 = 10, phi3 = 0),
      start_upper = c(phi1 = 2000, phi2 = 20, phi3 = 5),
      lower = c(phi1 = 1000, phi2 = 10, phi3 = 0),
      supp_errors = "Y"
    )))

  fit_boots <-
    fit_boots %>%
    mutate(pred = map(fit, ~ predict(., data.frame(fecha_index = c(max(mm$fecha_index) + 1, max(mm$fecha_index) + 2, max(mm$fecha_index) + 3)))[1:3]))


  # get predictions
  preds_boot <-
    fit_boots %>%
    mutate(preds = map(fit, augment)) %>%
    unnest(preds) %>%
    ungroup() %>%
    select(boot_num, fecha_index, .fitted)

  new_preds <-
    fit_boots %>%
    unnest(pred) %>%
    ungroup() %>%
    mutate(fecha_index = rep(
      c(max(mm$fecha_index) + 1, max(mm$fecha_index) + 2, max(mm$fecha_index) + 3),
      length(unique(.$boot_num))
    )) %>%
    select(boot_num, fecha_index, .fitted = pred)



  preds <-
    preds %>%
    left_join(mm[c("fecha_index", "fecha")])


  preds_boot <-
    preds_boot %>%
    left_join(mm[c("fecha_index", "fecha")])


  new_preds <-
    new_preds %>%
    left_join(new_pred[c("fecha", "fecha_index")])


  new_pred <-
    left_join(new_pred,
      new_preds %>%
        group_by(fecha_index) %>%
        mutate(
          lwr_CI = quantile(.fitted, 0.05),
          upr_CI = quantile(.fitted, 0.95),
          .fitted_med = quantile(.fitted, 0.5)
        ) %>%
        ungroup() %>%
        distinct(fecha_index, lwr_CI, upr_CI, .fitted_med),
      by = "fecha_index"
    )



  saveRDS(object = new_pred, file = here("data", "new_pred.rds"))
  saveRDS(object = preds, file = here("data", "preds.rds"))
  saveRDS(object = preds_boot, file = here("data", "preds_boot.rds"))
  saveRDS(object = new_preds, file = here("data", "new_preds.rds"))
  saveRDS(object = mm, file = here("data", "data_model.rds"))
}


if (run_model) build_model()
