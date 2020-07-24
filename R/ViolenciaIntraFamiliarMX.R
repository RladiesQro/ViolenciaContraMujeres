library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(sf)
source("VectoresAcentos.R")


CreaPaletaPersonalizada <- function(vector_usar, vector_colores, color_na = "#9C9C9C", numero_colores = 6) {
  custom.palette <- colorRampPalette(colors = vector_colores, space = "Lab")(numero_colores)
  pal <- colorNumeric(
    palette = custom.palette,
    domain = range(vector_usar, na.rm = T),
    na.color = color_na
  )
  return(pal)
}

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
    select(-c("Año", "name", "Clave_Ent")) %>%
    mutate_at(c("Entidad", "Bien_juridico_afectado", "Tipo", "Subtipo", "Modalidad"), factor)
}

GuardarDatosViolencia <- function(datos_violencia, nombre.archivo = "datos_violenciamx.rds") {
  previous.files <- list.files(path = ".", pattern = "datos_violenciamx.rds")
  if(is_empty(previous.files)){
    saveRDS(datos_violencia, nombre.archivo)
  } else {
    file.rename(previous.files, gsub(".rds", paste0("_", Sys.Date(), ".rds"), previous.files))
    saveRDS(datos_violencia, nombre.archivo)
  }
}

AgregaPoligonos <- function(data_with_entity, poligonos_mx) {
  data_with_entity %>%
    mutate(Entidad = as.character(Entidad)) %>%
    inner_join({
      poligonos_mx %>%
        select(Entidad = nombre, geometry) %>%
        mutate(Entidad = ifelse(Entidad == "Distrito Federal", "Ciudad de México", as.character(Entidad)))
    }, by = "Entidad") %>%
    st_as_sf()
}

AgregaTasaPoblacional <- function(resumen_datos_estatal, poblacion_inegi_2015, columna_a_tasa = "casos_por_estado") {
  resumen_datos_estatal %>%
    mutate(Entidad = as.character(Entidad)) %>%
    inner_join(poblacion_inegi_2015, by = "Entidad") %>%
    mutate(tasa_100k = (get(columna_a_tasa) / Habitantes2015) * 100000) %>%
    rename(poblacion_total = Habitantes2015)
}

CasosNormalizadosRepublica <- function(datos_violencia, poblacion_inegi_2015, filtro.tipo = NULL) {
  if(!is.null(filtro.tipo)) {
    violencia_normalizada <- datos_violencia %>%
      filter(Tipo == filtro.tipo)
  } else {
    violencia_normalizada <- datos_violencia
  }
  violencia_normalizada %>%
    group_by(Entidad) %>%
    summarise(casos_por_estado = sum(ocurrencia)) %>%
    mutate(Entidad = as.character(Entidad)) %>%
    AgregaTasaPoblacional(poblacion_inegi_2015, columna_a_tasa = "casos_por_estado") %>%
    mutate(casos_normalizados = scale(tasa_100k, center = FALSE)[, 1])
}

TasaPromedioMensual <- function(datos_violencia, poblacion_inegi_2015, filtro.tipo = NULL) {
  meses_sin_datos <- DefinirMesesSinDatos(datos_violencia)
  if(!is.null(filtro.tipo)) {
    violencia_normalizada <- datos_violencia %>%
      filter(Tipo == filtro.tipo)
  } else {
    violencia_normalizada <- datos_violencia
  }
  violencia_normalizada %>%
    filter(!floor_date(fecha, unit = "month") %in% meses_sin_datos) %>%
    group_by(Entidad, anyo = year(fecha), mes = month(fecha)) %>%
    summarise(casos_estado_mes = sum(ocurrencia)) %>%
    ungroup() %>%
    group_by(Entidad, anyo) %>%
    summarise(casos_promedio_mes = sum(casos_estado_mes) / n_distinct(mes)) %>%
    ungroup() %>%
    mutate(Entidad = as.character(Entidad)) %>%
    AgregaTasaPoblacional(., poblacion_inegi_2015, columna_a_tasa = "casos_promedio_mes")
}

ComparaMesesConDatos <- function(datos_violencia, filtro.tipo = NULL) {
  if(!is.null(filtro.tipo)) {
    violencia_normalizada <- datos_violencia %>%
      filter(Tipo == filtro.tipo)
  } else {
    violencia_normalizada <- datos_violencia
  }
  meses_sin_datos <- DefinirMesesSinDatos(datos_violencia)
  violencia_normalizada %>%
    group_by(Entidad, fecha = floor_date(fecha, unit = "month")) %>%
    summarise(casos_por_mes = sum(ocurrencia)) %>%
    ungroup() %>%
    filter(!month(fecha) %in% month(meses_sin_datos)) %>%
    AgregaTasaPoblacional(., poblacion_inegi_2015, columna_a_tasa = "casos_por_mes") %>%
    arrange(fecha)
}

DefinirMesesSinDatos <- function(datos_violencia) {
  datos_violencia %>%
    group_by(fecha = floor_date(fecha, unit = "month")) %>%
    summarise(frecuencia_casos = sum(ocurrencia)) %>%
    filter(frecuencia_casos == 0) %>%
    pull(fecha)
}
