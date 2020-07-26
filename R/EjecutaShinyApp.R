#' Ejecuta aplicación Shiny
#'
#' Ejecuta la aplicación shiny para la exploración de datos de
#' violencia contra la mujer.
#'
#' @return Aplicación shiny en función
#' @export
#'
#' @examples \dontrun{runShinyApp()}
ejecutaShinyApp <- function() {
  DirectorioApp <- system.file("app", package = "ViolenciaContraMujeres")
  if (DirectorioApp == "") {
    stop("No se encontro la applicación Shiny, prueba a reinstalar el paquete `ViolenciaContraMujeres`.", call. = FALSE)
  }
  shiny::runApp(DirectorioApp, display.mode = "normal")
}
