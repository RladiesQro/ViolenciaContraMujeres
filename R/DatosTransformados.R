#' Casos Normalizados de violencia contra la mujer por estado
#'
#' Calcula la tasa de casos de violencia contra la mujer por cada 100 mil habitantes
#' para cada estado. Ademas de que normaliza el valor de la tasa por cada 100 mil habitantes y permite
#' filtrar algún tipo de violencia para el calculo.
#'
#' @param datos_violencia Datos de violencia contra la mujer para la republica mexicana
#' @param poblacion_inegi_2015 Datos de población para cada estado tomados del Inegi 2015
#' @param filtro.tipo Cadena de texto con el Tipo por el que queremos filtrar los datos si no se
#' asigna o se deja NULL se usaran todos los tipos de violencia
#'
#' @return Resumen de datos de violencia por estado y normalizados.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' CasosNormalizadosRepublica(
#'   datos_violencia,
#'   poblacion_inegi_2015,
#'   filtro.tipo = "Violencia familiar"
#' )
CasosNormalizadosRepublica <- function(datos_violencia, poblacion_inegi_2015, filtro.tipo = NULL) {
  if(!is.null(filtro.tipo)) {
    violencia_normalizada <- datos_violencia %>%
      dplyr::filter(.data$Tipo == filtro.tipo)
  } else {
    violencia_normalizada <- datos_violencia
  }
  violencia_normalizada %>%
    dplyr::group_by(.data$Entidad) %>%
    dplyr::summarise(casos_por_estado = sum(.data$ocurrencia)) %>%
    dplyr::mutate(Entidad = as.character(.data$Entidad)) %>%
    AgregaTasaPoblacional(poblacion_inegi_2015, columna_a_tasa = "casos_por_estado") %>%
    dplyr::mutate(casos_normalizados = scale(.data$tasa_100k, center = FALSE)[, 1])
}

#' Tasa Promedio Mensual
#'
#' Calcula la tasa promedio mensual de los meses con información
#' dentro del conjunto de datos de violencia contra la mujer para México.
#' Permite hacer un filtrado por tipo de violencia.
#'
#' @param datos_violencia Datos de violencia contra la mujer para la republica mexicana
#' @param poblacion_inegi_2015 Población registrada para el año 2015
#' @param entidad Entidad para filtrar el conjunto de datos, si se declara "Todos" se calcula para
#' toda la republica, si se declara a NULL se calcula para cada estado.
#' @param filtro.tipo Filtro del tipo de violencia ocurrida, en caso de ser NULL se
#' usan todos los tipos de violencia
#'
#' @return Regresa un conjunto de datos con la tasa promedio por cada 100mil habitantes al mes.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' TasaPromedioMensual(datos_violencia, poblacion_inegi_2015)
#' TasaPromedioMensual(datos_violencia, poblacion_inegi_2015, filtro.tipo = "Violencia familiar")
#' TasaPromedioMensual(
#'   datos_violencia, poblacion_inegi_2015, entidad = "Querétaro", filtro.tipo = "Violencia familiar"
#' )
#' TasaPromedioMensual(
#'   datos_violencia, poblacion_inegi_2015, entidad = "Todos", filtro.tipo = "Violencia familiar"
#' )
TasaPromedioMensual <- function(datos_violencia, poblacion_inegi_2015, entidad = NULL, filtro.tipo = NULL) {
  meses_sin_datos <- DefinirMesesSinDatos(datos_violencia)
  violencia_normalizada <- datos_violencia
  if(!is.null(filtro.tipo) & !is.null(entidad)) {
    if(entidad == "Todos") {
      violencia_normalizada <- violencia_normalizada %>%
        dplyr::mutate(Entidad = "Todos")
    }
    violencia_normalizada <- violencia_normalizada %>%
      dplyr::filter(.data$Tipo == filtro.tipo, .data$Entidad == entidad)
  } else if(!is.null(filtro.tipo) & is.null(entidad)) {
    violencia_normalizada <- violencia_normalizada %>%
      dplyr::filter(.data$Tipo == filtro.tipo)
  } else if(is.null(filtro.tipo) & !is.null(entidad)) {
    if(entidad == "Todos") {
      violencia_normalizada <- violencia_normalizada %>%
        dplyr::mutate(Entidad = "Todos")
    }
    violencia_normalizada <- violencia_normalizada %>%
      dplyr::filter(.data$Entidad == entidad)
  }
  violencia_normalizada <- violencia_normalizada %>%
    dplyr::filter(!lubridate::floor_date(.data$fecha, unit = "month") %in% meses_sin_datos) %>%
    dplyr::group_by(.data$Entidad, anyo = lubridate::year(.data$fecha), mes = lubridate::month(.data$fecha)) %>%
    dplyr::summarise(casos_estado_mes = sum(.data$ocurrencia)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Entidad, .data$anyo) %>%
    dplyr::summarise(casos_promedio_mes = sum(.data$casos_estado_mes) / dplyr::n_distinct(.data$mes)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Entidad = as.character(.data$Entidad))
  if (!is.null(entidad)) {
    if(entidad == "Todos") {
      violencia_normalizada <- violencia_normalizada %>%
        dplyr::mutate(
          poblacion_total = sum(poblacion_inegi_2015$Habitantes2015),
          tasa_100k = .data$casos_promedio_mes / sum(poblacion_inegi_2015$Habitantes2015) * 100000
        )
      return(violencia_normalizada)
    }
  }
  violencia_normalizada %>%
    AgregaTasaPoblacional(poblacion_inegi_2015, columna_a_tasa = "casos_promedio_mes")
}

#' Comparativa de Meses con Datos
#'
#' Genera un dataframe con los meses que presentan datos en todos los
#' años existentes en el conjunto de datos. La ocurrencia de casos se representa con
#' la tasa por cada 100 mil habitantes. Ademas permite filtrar los casos de violencia
#' por el tipo.
#'
#' @param datos_violencia Conjunto de datos con el conteo de casos de violecina en méxico
#' @param filtro.tipo Cadena de texto con el tipo de violencia a filtrar
#' @param poblacion_inegi_2015 Población registrada para el año 2015
#'
#' @return Genera un conjunto de datos que compara solo los meses con información
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' ComparaMesesConDatos(
#'   datos_violencia,
#'   poblacion_inegi_2015,
#'   filtro.tipo = "Violencia familiar"
#' )
ComparaMesesConDatos <- function(datos_violencia, poblacion_inegi_2015, filtro.tipo = NULL) {
  if(!is.null(filtro.tipo)) {
    violencia_normalizada <- datos_violencia %>%
      dplyr::filter(.data$Tipo == filtro.tipo)
  } else {
    violencia_normalizada <- datos_violencia
  }
  meses_sin_datos <- DefinirMesesSinDatos(datos_violencia)
  violencia_normalizada %>%
    dplyr::group_by(.data$Entidad, fecha = lubridate::floor_date(.data$fecha, unit = "month")) %>%
    dplyr::summarise(casos_por_mes = sum(.data$ocurrencia)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!lubridate::month(.data$fecha) %in% lubridate::month(meses_sin_datos)) %>%
    AgregaTasaPoblacional(poblacion_inegi_2015, columna_a_tasa = "casos_por_mes") %>%
    dplyr::arrange(.data$fecha)
}

#' Datos de Meses por Estado Agrupados popr tipo
#'
#' Esta función genera un dataset con los datos de meses que estan completos en el todos los años.
#' Estos datos los muestra para un estado en específico. De seleccionar \strong{Todos} se mostraran los datos
#' de toda la republica. Ademas esta funcion permite seleccionar el tipo de violencia a la que se le va
#' a dar mayor relevancia. Es decir si la opcion \code{resaltar.tipo} esta a NULL se incluyen todos los tipos,
#' si se agrega un vector, estos tipos se mantendran y el resto de tipos de violencia se agruparan con la etiqueta
#' "Otros tipos de violencia".
#'
#' @param datos_violencia Datos de violencia contra la mujer
#' @param entidad Entidad que se desea obtener, si se coloca "Todas" se obtienen los datos de toda la republica
#' @param resaltar.tipo Vector con el tipo de violencia que se mantendra, el resto se agrupa en un grupo.
#' Sí se coloca NULL se obtendran todos los tipos.
#'
#' @return Conjunto de datos con la Entidad seleccionada, El tipo de de violencia, año, mes,
#' casos registrados para el tipo de violencia, casos registrados totales para la entidad, mes y año.
#' Y porcentaje que este tipo representa.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' DatosMesEstadoAgrupados(
#'   datos_violencia,  entidad = "Todas", resaltar.tipo = c("Violencia familiar", "Abuso sexual")
#' )
DatosMesEstadoAgrupados <- function(datos_violencia, entidad = NULL, resaltar.tipo = NULL) {
  violencia_mes_entidad <- datos_violencia
  if (!is.null(entidad)) {
    if (entidad == "Todas") {
    violencia_mes_entidad <- violencia_mes_entidad %>%
      dplyr::mutate(Entidad = "Todas")
    } else {
      violencia_mes_entidad <- violencia_mes_entidad %>%
        dplyr::filter(.data$Entidad == entidad)
    }
  }
  if (!is.null(resaltar.tipo)){
    violencia_mes_entidad <- violencia_mes_entidad %>%
      dplyr::mutate(Tipo = as.character(.data$Tipo)) %>%
      dplyr::mutate(Tipo = ifelse(.data$Tipo %in% resaltar.tipo, .data$Tipo, "Otros tipos de Violencia" ))
  }
  meses_sin_datos <- DefinirMesesSinDatos(datos_violencia)
  violencia_mes_entidad <- violencia_mes_entidad %>%
    dplyr::filter(!lubridate::month(.data$fecha) %in% lubridate::month(meses_sin_datos)) %>%
    dplyr::group_by(.data$Entidad, .data$Tipo, anyo = lubridate::year(.data$fecha), mes = lubridate::month(.data$fecha, label = TRUE)) %>%
    dplyr::summarise(casos_tipo = sum(.data$ocurrencia)) %>%
    dplyr::group_by(.data$Entidad, .data$anyo, .data$mes) %>%
    dplyr::mutate(casos_registrados = sum(.data$casos_tipo)) %>%
    dplyr::mutate(proporcion_tipo = .data$casos_tipo / .data$casos_registrados)
}


#' Calculate ranking de tipo de violencia contra la mujer
#'
#' Calcula la posición de cada tipo de violencia contra la mujer con base en la cuenta total de
#' casos por año por entidad, en caso de colocar a NULL la entidad o no declarar el parámetro se calcula
#' a total toda la republica mexicana.
#'
#' @param datos_violencia Datos de casos de violencia contra la mujer
#' @param entidad Entidad para filtrar el dataset, en caso de no declararlo o ponerlo a nul se calcula a todo México
#' @param numero_posiciones Posiciones que se muestran dentro del ranking, por año y tipo de violencia.
#'
#' @return Conjunto de datos con la columna, Tipo de caso, año, número de casos y psoición dentro del rank
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples  RankingTipoViolencia(datos_violencia, entidad = "Querétaro", numero_posiciones = 5)
RankingTipoViolencia <- function(datos_violencia, entidad = NULL, numero_posiciones = NULL) {
  if(!is.null(entidad)) {
    ranking_tipo_violencia <- datos_violencia %>%
      dplyr::filter(.data$Entidad == entidad)
  } else {
    ranking_tipo_violencia <- datos_violencia
  }
  ranking_tipo_violencia <- ranking_tipo_violencia %>%
    dplyr::group_by(.data$Tipo, anyo = lubridate::year(.data$fecha)) %>%
    dplyr::summarise(numero_casos = sum(.data$ocurrencia)) %>%
    dplyr::arrange(.data$anyo, dplyr::desc(.data$numero_casos)) %>%
    dplyr::group_by(.data$anyo)
  if (!is.null(numero_posiciones)) {
    ranking_tipo_violencia <- ranking_tipo_violencia %>%
      dplyr::top_n(numero_posiciones)
  }
  ranking_tipo_violencia %>%
    dplyr::mutate(rank = dplyr::row_number())
}
