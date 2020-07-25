#' Datos de violencia en México
#'
#' Este conjunto de datos presenta el conteo de casos para diferentes tipos de violencia,
#' con información de fecha, entidad, tipo, subtipo, modalidad y Bien Jurídico involucrado.
#'
#' @format Un tibble con 7 columnas y 225792 renglones:
#' \describe{
#'   \item{Entidad}{Estado de la republica mexicana}
#'   \item{Bien_juridico_afectado}{Los bienes involucrados en el eacto de violencia}
#'   \item{Tipo}{Tipo de evento violento registrado}
#'   \item{Subtipo}{Subtipo de violencia}
#'   \item{Modalidad}{Modo con el que se llevo a cabo el acto de violencia registrada}
#'   \item{ocurrencia}{Número de casos registrados para el estado de ese tipo, modalidad y fecha}
#'   \item{fecha}{Fecha de registro del evento de violencia}
#' }
"datos_violencia"

#' Registro de población estatal 2015
#'
#' Registro de la población por estado reportado por el INEGI para el año 2015
#'
#' @format Un tibble con 2 columnas y 32 renglones:
#' \describe{
#'   \item{Entidad}{Estado de la republica mexicana}
#'   \item{Habitantes2015}{Número de habitantes registrados por el INEGI para el año 2015}
#' }
"poblacion_inegi_2015"

#' Poligonos de Estados de la Republica Mexicana
#'
#' Este set de datos presenta los polígonos para los estados de la republica mexicana
#'
#' @format Un objeto de clase sf con la relación de poligonos/entidad
#' \describe{
#'   \item{id}{Identificador de registro}
#'   \item{cvegeo}{Identificador de la entidad federativa}
#'   \item{nombre}{Nombre de la entidad federativa}
#'   \item{geometry}{Coordenadas geográficas que generan el poligono de la entidad}
#' }
"poligonos_mx"
