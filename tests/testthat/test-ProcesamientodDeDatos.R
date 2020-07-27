test_that("Creación de paleta de colores", {
  paleta_creada <- CreaPaletaPersonalizada(1:4, c("#2F394D", "#EEE1B3"))
  expect_type(paleta_creada, "closure")
  expect_equal(paleta_creada(1:4), c("#2E394C", "#6C6C6D", "#ABA590", "#EDE0B2"))
  expect_equal(paleta_creada(3), "#ABA590")
  expect_warning(paleta_creada(5))
})

test_that("Meses sin datos", {
  meses_sin_datos <- DefinirMesesSinDatos(datos_violencia)
  expect_equal(
    as.character(meses_sin_datos),
    c("2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")
  )
  expect_equal(datos_violencia %>%
    dplyr::filter(fecha %in% meses_sin_datos) %>%
      dplyr::pull(ocurrencia) %>%
      sum(),
    0
  )
})

test_that("Agrega poligonos", {
  datos_poligonos <- AgregaPoligonos(datos_violencia, poligonos_mx)
  expect_true(
    all(datos_poligonos$Entidad %in% c(
      "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Ciudad de México",
      "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán de Ocampo",
      "Morelos", "México", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
      "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
      "Yucatán", "Zacatecas"))
  )
  expect_s3_class(datos_poligonos, "sf")
  expect_s3_class(datos_poligonos$fecha, "Date")
  expect_equal(attr(datos_poligonos, "sf_column"), "geometry")
  expect_equal(as.numeric(sf::st_bbox(datos_poligonos)), c(-118.40765, 14.53210, -86.71041,32.71865), tolerance = 1e-6)
})

test_that("Agrega población", {
  datos_corregidos_tasa <- AgregaTasaPoblacional(
    {
      datos_violencia %>%
        dplyr::group_by(Entidad) %>%
        dplyr::summarise(casos_totales = sum(ocurrencia))
    },
    poblacion_inegi_2015,
    columna_a_tasa = "casos_totales"
  )
  expect_named(datos_corregidos_tasa, c("Entidad", "casos_totales", "poblacion_total", "tasa_100k"))
  expect_true(
    all(datos_corregidos_tasa$Entidad %in% c(
      "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Ciudad de México",
      "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán de Ocampo",
      "Morelos", "México", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
      "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
      "Yucatán", "Zacatecas"))
  )
  expect_length(datos_corregidos_tasa$Entidad, 32)
  expect_equal(sum(datos_corregidos_tasa$casos_totales), 9909757)
  expect_equal(sum(datos_corregidos_tasa$poblacion_total), 119530753)
  expect_equal(sum(datos_corregidos_tasa$tasa_100k), 269614.3, tolerance = 1e-3)
})
