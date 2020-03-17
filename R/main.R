library(here)
library(tidyverse)
library(geojsonsf)
library(sf)
library(leaflet)
library(tmap)
library(leafpop)



canarias <- geojson_sf(here('data', 'ISLANDS.geojson'))
data_covid <- read_csv2(here('data', 'covid_canarias.csv'))
data_covid_filt <- filter(data_covid, fecha == max(fecha))
canarias_coord <- read_delim(here('data', 'islas_coordenadas.csv'), delim = ';') 


canarias <- 
  canarias %>% 
  left_join(data_covid_filt[c('isla','casos_activos','fallecimientos')], by = c('label' = 'isla')) %>% 
  left_join(canarias_coord, by = c('label' = 'isla')) %>% 
  rename(isla = label)



pal <- colorNumeric(
  palette = "Reds",
  domain = canarias$casos_activos)


labels <- sprintf(
  "<strong>%s</strong><br/>%g casos activos",
  canarias$isla, canarias$casos_activos) %>% 
  lapply(htmltools::HTML)


