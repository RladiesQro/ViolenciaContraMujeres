
#' Crae Paleta de colores personalizada
#'
#' Esta función permite crear un vector con los colores entre 2 o 3 colores a
#' partir de un vector de datos numéricos para ser asignados al mismo.
#'
#' @param vector_usar Vector de datos a los que se le genera un color asignado
#' @param vector_colores Vector de dos o mas colores usados para generar la escala de colores
#' @param color_na Color asignado a valores NA
#' @param numero_colores Variación de colores a usar para generar la escala final
#'
#' @return Función que genera una escala de colores
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples paleta.colores <- CreaPaletaPersonalizada(1:5, c("#2F394D", "#EEE1B3"))
CreaPaletaPersonalizada <- function(vector_usar, vector_colores, color_na = "#9C9C9C", numero_colores = 6) {
  custom.palette <- grDevices::colorRampPalette(colors = vector_colores, space = "Lab")(numero_colores)
  pal <- leaflet::colorNumeric(
    palette = custom.palette,
    domain = range(vector_usar, na.rm = T),
    na.color = color_na
  )
  return(pal)
}

#' Definir Meses sin datos registrados
#'
#' Detecta los meses del dataset que no tienen registros de violencia contra la mujer, en
#' general puede deberse a años incompletos en información.
#'
#' @param datos_violencia Dataset de violencia procesado
#'
#' @return Vector con las fechas del tipo "2019-11-01"
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples DefinirMesesSinDatos(datos_violencia)
DefinirMesesSinDatos <- function(datos_violencia) {
  datos_violencia %>%
    dplyr::group_by(fecha = lubridate::floor_date(.data$fecha, unit = "month")) %>%
    dplyr::summarise(frecuencia_casos = sum(.data$ocurrencia)) %>%
    dplyr::filter(.data$frecuencia_casos == 0) %>%
    dplyr::pull(.data$fecha)
}

#' Agrega información de poligonos a los datos
#'
#' Agrega a los datos originales una nueva columna de geometría con los polígonos por estado y
#' Convierte el set de datos a un objeto SF específico para información georeferenciada.
#'
#' @param data_with_entity DataFrame, tibble e incluso data.table de datos con una columna
#' con la identidad que se necesita obtener
#' @param poligonos_mx Objeto SF con los polígonos y una columna de entidad compatible con los datos por entidad
#'
#' @return Objeto sf con los datos por entidad y una columna con los polígonos
#' @export
#'
#' @importFrom rlang .data
#' @import sf
#'
#' @examples AgregaPoligonos(datos_violencia, poligonos_mx)
AgregaPoligonos <- function(data_with_entity, poligonos_mx) {
  data_with_entity %>%
    dplyr::mutate(Entidad = as.character(.data$Entidad)) %>%
    dplyr::inner_join({
      poligonos_mx %>%
        dplyr::select(Entidad = .data$nombre, .data$geometry)
    }, by = "Entidad") %>%
    sf::st_as_sf()
}

#' Calcula tasa poblacional
#'
#' Usando los datos de población por estado para el año 2015 se calcula la tasa
#' de casos por cada 100,000 habitantes y permitir una comparativa un tanto ponderada
#'
#' @param resumen_datos_estatal Datos con los casos de violencia contra la mujer por estado
#' @param poblacion_inegi_2015 Set de datos con la población total por estado
#' @param columna_a_tasa Cadena de tecto con el nombre de la olumna con los casos de violencia que se transforman a tasa
#'
#' @return Datos con dos columnas extras tasa por cada 100 mil habitantes y habitantes totales para ese estado.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'   AgregaTasaPoblacional(
#'     resumen_datos_estatal,
#'     poblacion_inegi_2015,
#'     columna_a_tasa = "casos_por_estado"
#'   )
#' }
AgregaTasaPoblacional <- function(resumen_datos_estatal, poblacion_inegi_2015, columna_a_tasa = "casos_por_estado") {
  resumen_datos_estatal %>%
    dplyr::mutate(Entidad = as.character(.data$Entidad)) %>%
    dplyr::inner_join(poblacion_inegi_2015, by = "Entidad") %>%
    dplyr::mutate(tasa_100k = (get(columna_a_tasa) / .data$Habitantes2015) * 100000) %>%
    dplyr::rename(poblacion_total = .data$Habitantes2015)
}
