library(ggplot2)
library(glue)
library(leaflet)
library(lubridate)
library(plotly)
library(scales)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(stringr)
library(sf)
library(purrr)
library(ViolenciaIntrafamiliar)


violencia_anual <- TasaPromedioMensual(datos_violencia, poblacion_inegi_2015)
violencia_familiar_anual <- TasaPromedioMensual(datos_violencia, poblacion_inegi_2015, "Violencia familiar")
violencia_meses_con_datos <- ComparaMesesConDatos(datos_violencia, poblacion_inegi_2015, "Violencia familiar")
vector_estados <- datos_violencia %>%
  select(Entidad) %>%
  distinct() %>%
  arrange(Entidad) %>%
  mutate(Entidad = as.character(Entidad))
