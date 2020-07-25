#' Casos Normalizados de violencia por estado
#'
#' Calcula la tasa de casos de violencia por cada 100 mil habitantes para cada estado
#' Ademas de que normaliza el valor de la tasa por cada 100 mil habitantes y permite
#' filtrar algún tipo de violencia para el calculo.
#'
#' @param datos_violencia Datos de violencia para la republica mexicana
#' @param poblacion_inegi_2015 Datos de población para cada estado tomados del Inegi 2015
#' @param filtro.tipo Cadena de texto con el Tipo por el que queremos filtrar los datos si no se
#' asigna o se deja NULL se usaran todos los tipos de violencia
#'
#' @return Resumen de datos de violencia por estado y normalizados.
#' @export
#'
#' @examples
#' CasosNormalizadosRepublica(datos_violencia, poblacion_inegi_2015, filtro.tipo = "Violencia familiar")
CasosNormalizadosRepublica <- function(datos_violencia, poblacion_inegi_2015, filtro.tipo = NULL) {
  if(!is.null(filtro.tipo)) {
    violencia_normalizada <- datos_violencia %>%
      dplyr::filter(Tipo == filtro.tipo)
  } else {
    violencia_normalizada <- datos_violencia
  }
  violencia_normalizada %>%
    dplyr::group_by(Entidad) %>%
    dplyr::summarise(casos_por_estado = sum(ocurrencia)) %>%
    dplyr::mutate(Entidad = as.character(Entidad)) %>%
    AgregaTasaPoblacional(poblacion_inegi_2015, columna_a_tasa = "casos_por_estado") %>%
    dplyr::mutate(casos_normalizados = scale(tasa_100k, center = FALSE)[, 1])
}

#' Tasa Promedio Mensual
#'
#' Calcula la tasa promedio mensual de los meses con información
#' dentro del conjunto de datos de violencia para México.
#' Permite hacer un filtrado por tipo de violencia.
#'
#' @param datos_violencia Datos de violencia para la republica mexicana
#' @param poblacion_inegi_2015 Población registrada para el año 2015
#' @param filtro.tipo Filtro del tipo de violencia ocurrida, en caso de ser NULL se
#' usan todos los tipos de violencia
#'
#' @return Regresa un conjunto de datos con la tasa promedio por cada 100mil habitantes al mes.
#' @export
#'
#' @examples
#' TasaPromedioMensual(datos_violencia, poblacion_inegi_2015, filtro.tipo = "Violencia familiar")
TasaPromedioMensual <- function(datos_violencia, poblacion_inegi_2015, filtro.tipo = NULL) {
  meses_sin_datos <- DefinirMesesSinDatos(datos_violencia)
  if(!is.null(filtro.tipo)) {
    violencia_normalizada <- datos_violencia %>%
      dplyr::filter(Tipo == filtro.tipo)
  } else {
    violencia_normalizada <- datos_violencia
  }
  violencia_normalizada %>%
    dplyr::filter(!lubridate::floor_date(fecha, unit = "month") %in% meses_sin_datos) %>%
    dplyr::group_by(Entidad, anyo = lubridate::year(fecha), mes = lubridate::month(fecha)) %>%
    dplyr::summarise(casos_estado_mes = sum(ocurrencia)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Entidad, anyo) %>%
    dplyr::summarise(casos_promedio_mes = sum(casos_estado_mes) / dplyr::n_distinct(mes)) %>%
    dplyr::ungroup() %>%
    mutate(Entidad = as.character(Entidad)) %>%
    AgregaTasaPoblacional(., poblacion_inegi_2015, columna_a_tasa = "casos_promedio_mes")
}

#' Comparativa de Meses con Datos
#'
#' Genera un conjunto de datos con los meses que presentan datos en todos los
#' años existentes en el conjunto de datos. La ocurrencia de casos se representa con
#' la tasa por cada 100 mil habitantes. Ademas permite filtrar los casos de violencia
#' por el tipo.
#'
#' @param datos_violencia Conjunto de datos con el conteo de casos de violecina en méxico
#' @param filtro.tipo Cadena de texto con el tipo de violencia a filtrar
#'
#' @return Genera un conjunto de datos que compara solo los meses con información
#' @export
#'
#' @examples
#' ComparaMesesConDatos(datos_violencia, filtro.tipo = "Violencia familiar")
ComparaMesesConDatos <- function(datos_violencia, filtro.tipo = NULL) {
  if(!is.null(filtro.tipo)) {
    violencia_normalizada <- datos_violencia %>%
      dplyr::filter(Tipo == filtro.tipo)
  } else {
    violencia_normalizada <- datos_violencia
  }
  meses_sin_datos <- DefinirMesesSinDatos(datos_violencia)
  violencia_normalizada %>%
    dplyr::group_by(Entidad, fecha = lubridate::floor_date(fecha, unit = "month")) %>%
    dplyr::summarise(casos_por_mes = sum(ocurrencia)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!lubridate::month(fecha) %in% lubridate::month(meses_sin_datos)) %>%
    AgregaTasaPoblacional(., poblacion_inegi_2015, columna_a_tasa = "casos_por_mes") %>%
    dplyr::arrange(fecha)
}
