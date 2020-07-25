source("VectoresAcentos.R")
datos_violencia <- readr::read_csv("violencia_mexico.csv") %>%
  dplyr::rename(Bien_juridico_afectado = `Bien juridico afectado`) %>%
  tidyr::pivot_longer(Enero:Diciembre, values_to = "ocurrencia") %>%
  dplyr::mutate(
    Entidad = purrr::map_chr(Entidad, function(x) ifelse(is.na(vector.fix.entidad[x]), x, vector.fix.entidad[x])),
    Bien_juridico_afectado = purrr::map_chr(
      Bien_juridico_afectado,
      function(x) ifelse(is.na(vector.fix.bien.juridico[x]), x, vector.fix.bien.juridico[x])
    ),
    Tipo = purrr::map_chr(Tipo, function(x) ifelse(is.na(vector.fix.tipo[x]), x, vector.fix.tipo[x])),
    Subtipo = purrr::map_chr(Subtipo, function(x) ifelse(is.na(vector.fix.subtipo[x]), x, vector.fix.subtipo[x])),
    Modalidad = purrr::map_chr(Modalidad, function(x) ifelse(is.na(vector.fix.modalidad[x]), x, vector.fix.modalidad[x])),
    fecha = as.Date(paste("01", name, Año), format = "%d %B %Y"),
    Clave_Ent = sprintf("%02d", Clave_Ent),
    ocurrencia = ifelse(is.na(ocurrencia), 0, ocurrencia)
  ) %>%
  select(-c("Año", "name", "Clave_Ent")) %>%
  mutate_at(c("Entidad", "Bien_juridico_afectado", "Tipo", "Subtipo", "Modalidad"), factor)
usethis::use_data(datos_violencia, overwrite = TRUE)

poligonos_mx <- sf::st_read("mexico_estados.json")
usethis::use_data(poligonos_mx, overwrite = TRUE)

poblacion_inegi_2015 <- readr::read_csv("PoblacionEntidadInegi2015.csv")
usethis::use_data(poblacion_inegi_2015, overwrite = TRUE)
