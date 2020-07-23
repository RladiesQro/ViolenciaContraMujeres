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
      Bien_juridico_afectado = map_chr(
        Bien_juridico_afectado,
        function(x) ifelse(is.na(vector.fix.bien.juridico[x]), x, vector.fix.bien.juridico[x])
      ),
      Tipo = map_chr(Tipo, function(x) ifelse(is.na(vector.fix.tipo[x]), x, vector.fix.tipo[x])),
      Subtipo = map_chr(Subtipo, function(x) ifelse(is.na(vector.fix.subtipo[x]), x, vector.fix.subtipo[x])),
      Modalidad = map_chr(Modalidad, function(x) ifelse(is.na(vector.fix.modalidad[x]), x, vector.fix.modalidad[x])),
      fecha = as.Date(paste("01", name, Año), format = "%d %B %Y"),
      Clave_Ent = sprintf("%02d", Clave_Ent),
      ocurrencia = ifelse(is.na(ocurrencia), 0, ocurrencia)
    ) %>%
    inner_join({
      st_read("mexico_estados.json") %>%
        select(Clave_Ent = cvegeo, geometry) %>%
        mutate(Clave_Ent = as.character(Clave_Ent))
    }, by = "Clave_Ent") %>%
    inner_join(read_csv("PoblacionEntidadInegi2015.csv"), by = "Entidad") %>%
    select(-c("Año", "Clave_Ent", "name")) %>%
    mutate_at(c("Entidad", "Bien_juridico_afectado", "Tipo", "Subtipo", "Modalidad"), factor)
}

GuardarDatosViolencia <- function(datos_violencia, nombre.archivo = "datos_violenciamx.rds") {
  previous.files <- list.files("datos_violenciamx.rds")
  if(is_empty(previous.files)){
    saveRDS(datos_violencia, nombre.archivo)
  } else {
    file.rename(previous.files, gsub(".rds", paste0("_", Sys.Date(), ".rds"), previous.files))
    saveRDS(datos_violencia, nombre.archivo)
  }
}

CasosNormalizadosRepublica <- function(datos_violencia, filtro.tipo = NULL) {
  if(!is.null(filtro.tipo)) {
    violencia_normalizada <- datos_violencia %>%
      filter(Tipo == filtro.tipo)
  } else {
    violencia_normalizada <- datos_violencia
  }
  violencia_normalizada <- violencia_normalizada %>%
    group_by(Entidad) %>%
    summarise(casos_por_estado = sum(ocurrencia)) %>%
    mutate(casos_normalizados = scale(casos_por_estado, center = FALSE)[, 1])

  return(violencia_normalizada)
}

CreaPaletaPersonalizada <- function(vector_usar, vector_colores, color_na = "#9C9C9C", numero_colores = 6) {
  custom.palette <- colorRampPalette(colors = vector_colores, space = "Lab")(numero_colores)
  pal <- colorNumeric(
    palette = custom.palette,
    domain = range(vector_usar, na.rm = T),
    na.color = color_na
  )
  return(pal)
}
