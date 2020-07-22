library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(sf)
source("VectoresAcentos.R")


LeerDatos <- function(archivo = "violencia_mexico.csv", ruta = ".") {
  ruta.archivo <- file.path(ruta, archivo)
  read_csv(ruta.archivo) %>%
    rename(Bien_juridico_afectado = `Bien juridico afectado`) %>%
    pivot_longer(Enero:Diciembre, values_to = "ocurrencia") %>%
    mutate(
      Entidad = map_chr(Entidad, function(x) ifelse(is.na(vector.fix.entidad[x]), x, vector.fix.entidad[x])),
      Bien_juridico_afectado = map_chr(Bien_juridico_afectado, function(x) ifelse(is.na(vector.fix.bienjur[x]), x, vector.fix.bienjur[x])),
      Tipo = map_chr(Tipo, function(x) ifelse(is.na(vector.fix.tipo[x]), x, vector.fix.tipo[x])),
      Subtipo = map_chr(Subtipo, function(x) ifelse(is.na(vector.fix.subtipo[x]), x, vector.fix.subtipo[x])),
      Modalidad = map_chr(Modalidad, function(x) ifelse(is.na(vector.fix.modalidad[x]), x, vector.fix.modalidad[x])),
      fecha = as.Date(paste("01", name, Año), format = "%d %B %Y"),
      Clave_Ent = sprintf("%02d", Clave_Ent)
    ) %>%
    inner_join({
      st_read("mexico_estados.json") %>%
        select(Clave_Ent = cvegeo, geometry)
    }, by = "Clave_Ent") %>%
    select(-c("Año", "Clave_Ent", "name")) %>%
    mutate_at(c("Entidad", "Bien_juridico_afectado", "Tipo", "Subtipo", "Modalidad"), factor)
}



