datos_violencia <- readr::read_csv("violencia_mexico.csv") %>%
  dplyr::rename(Bien_juridico_afectado = `Bien juridico afectado`) %>%
  tidyr::pivot_longer(Enero:Diciembre, values_to = "ocurrencia") %>%
  dplyr::mutate(
    fecha = as.Date(paste("01", name, Año), format = "%d %B %Y"),
    Clave_Ent = sprintf("%02d", Clave_Ent),
    ocurrencia = ifelse(is.na(ocurrencia), 0, ocurrencia)
  ) %>%
  dplyr::select(-c("Año", "name", "Clave_Ent")) %>%
  dplyr::mutate_at(c("Entidad", "Bien_juridico_afectado", "Tipo", "Subtipo", "Modalidad"), factor)
usethis::use_data(datos_violencia, overwrite = TRUE)

poligonos_mx <- sf::st_read("mexico_estados.json")
usethis::use_data(poligonos_mx, overwrite = TRUE)

poblacion_inegi_2015 <- readr::read_csv("PoblacionEntidadInegi2015.csv")
usethis::use_data(poblacion_inegi_2015, overwrite = TRUE)
