library(ggplot2)
library(glue)
library(leaflet)
library(lubridate)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tictoc)
source("ViolenciaIntraFamilairMX.R")


tic("Carga de datos")
datos_violencia <- readRDS("datos_violenciamx.rds")
toc()
vector_estados <- datos_violencia %>%
  select(Entidad) %>%
  distinct() %>%
  arrange(Entidad) %>%
  mutate(Entidad = as.character(Entidad))
