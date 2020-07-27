test_that("Integridad casos normalizados", {
  casos_normalizados <- CasosNormalizadosRepublica(datos_violencia, poblacion_inegi_2015)
  expect_length(casos_normalizados$Entidad, 32)
  expect_true(
    all(casos_normalizados$Entidad %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Ciudad de México",
      "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán de Ocampo",
      "Morelos", "México", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
      "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
      "Yucatán", "Zacatecas"))
  )
  expect_equal(sum(casos_normalizados$casos_por_estado),  9909757)
  expect_equal(sum(casos_normalizados$tasa_100k),  269614.3, tolerance = 1e-3)
  expect_equal(sum(casos_normalizados$poblacion_total),  119530753)
})

test_that("Integridad casos normalizados con filtro Violencia Familiar", {
  casos_normalizados <- CasosNormalizadosRepublica(
    datos_violencia, poblacion_inegi_2015, filtro.tipo = "Violencia familiar"
  )
  expect_length(casos_normalizados$Entidad, 32)
  expect_true(
    all(casos_normalizados$Entidad %in% c(
      "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Ciudad de México",
      "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán de Ocampo",
      "Morelos", "México", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
      "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
      "Yucatán", "Zacatecas"))
  )
  expect_equal(sum(casos_normalizados$casos_por_estado),  919736)
  expect_equal(sum(casos_normalizados$tasa_100k),  27048.77, tolerance = 1e-3)
  expect_equal(sum(casos_normalizados$poblacion_total),  119530753)
})

test_that("Integridad Tasa Promedio Mensual", {
  tasa_promedio_mensual <- TasaPromedioMensual(datos_violencia, poblacion_inegi_2015)
  expect_length(unique(tasa_promedio_mensual$Entidad), 32)
  expect_true(
    all(tasa_promedio_mensual$Entidad %in% c(
      "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Ciudad de México",
      "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán de Ocampo",
      "Morelos", "México", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
      "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
      "Yucatán", "Zacatecas"))
  )
  expect_equal(sum(tasa_promedio_mensual$casos_promedio_mes),  912886.8, tolerance = 1e-3)
  expect_equal(sum(tasa_promedio_mensual$tasa_100k), 24785.4, tolerance = 1e-3)
  expect_equal(
    tasa_promedio_mensual %>%
      dplyr::distinct(Entidad, poblacion_total) %>%
      dplyr::pull(poblacion_total) %>%
      sum(),
    119530753
  )
  expect_equal(unique(tasa_promedio_mensual$anyo), 2015:2020)
})

test_that("Integridad Tasa Promedio Mensual con filtro de Violencia Familiar", {
  tasa_promedio_mensual <- TasaPromedioMensual(datos_violencia, poblacion_inegi_2015, filtro.tipo = "Violencia familiar")
  expect_length(unique(tasa_promedio_mensual$Entidad), 32)
  expect_true(
    all(tasa_promedio_mensual$Entidad %in% c(
      "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Ciudad de México",
      "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán de Ocampo",
      "Morelos", "México", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
      "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
      "Yucatán", "Zacatecas"))
  )
  expect_equal(sum(tasa_promedio_mensual$casos_promedio_mes),  86613.25, tolerance = 1e-3)
  expect_equal(sum(tasa_promedio_mensual$tasa_100k), 2548.688, tolerance = 1e-3)
  expect_equal(
    tasa_promedio_mensual %>%
      dplyr::distinct(Entidad, poblacion_total) %>%
      dplyr::pull(poblacion_total) %>%
      sum(),
    119530753
  )
  expect_equal(unique(tasa_promedio_mensual$anyo), 2015:2020)
})

test_that("Datos para comparativa de meses con datos", {
  comparativa_meses_condatos <- ComparaMesesConDatos(datos_violencia, poblacion_inegi_2015)
  expect_length(unique(comparativa_meses_condatos$Entidad), 32)
  expect_true(
    all(comparativa_meses_condatos$Entidad %in% c(
      "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Ciudad de México",
      "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán de Ocampo",
      "Morelos", "México", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
      "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
      "Yucatán", "Zacatecas"))
  )
  expect_equal(sum(comparativa_meses_condatos$casos_por_mes), 4484350)
  expect_equal(sum(comparativa_meses_condatos$tasa_100k), 122329.7, tolerance = 1e-3)
  expect_equal(
    comparativa_meses_condatos %>%
      dplyr::distinct(Entidad, poblacion_total) %>%
      dplyr::pull(poblacion_total) %>%
      sum(),
    119530753
  )
  expect_equal(
    as.character(sort(unique(comparativa_meses_condatos$fecha))),
    c("2015-01-01", "2015-02-01", "2015-03-01", "2015-04-01", "2015-05-01", "2016-01-01", "2016-02-01", "2016-03-01",
      "2016-04-01", "2016-05-01", "2017-01-01", "2017-02-01", "2017-03-01", "2017-04-01", "2017-05-01", "2018-01-01",
      "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01", "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01",
      "2019-05-01", "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01")
  )
  expect_s3_class(comparativa_meses_condatos$fecha, "Date")
})

test_that("Datos para comparativa de meses con datos y filtro de Violencia Familiar", {
  comparativa_meses_condatos <- ComparaMesesConDatos(datos_violencia, poblacion_inegi_2015, filtro.tipo = "Violencia familiar")
  expect_length(unique(comparativa_meses_condatos$Entidad), 32)
  expect_true(
    all(comparativa_meses_condatos$Entidad %in% c(
      "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Ciudad de México",
      "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán de Ocampo",
      "Morelos", "México", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
      "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
      "Yucatán", "Zacatecas"))
  )
  expect_equal(sum(comparativa_meses_condatos$casos_por_mes), 421752)
  expect_equal(sum(comparativa_meses_condatos$tasa_100k), 12380.16, tolerance = 1e-3)
  expect_equal(
    comparativa_meses_condatos %>%
      dplyr::distinct(Entidad, poblacion_total) %>%
      dplyr::pull(poblacion_total) %>%
      sum(),
    119530753
  )
  expect_equal(
    as.character(sort(unique(comparativa_meses_condatos$fecha))),
    c("2015-01-01", "2015-02-01", "2015-03-01", "2015-04-01", "2015-05-01", "2016-01-01", "2016-02-01", "2016-03-01",
      "2016-04-01", "2016-05-01", "2017-01-01", "2017-02-01", "2017-03-01", "2017-04-01", "2017-05-01", "2018-01-01",
      "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01", "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01",
      "2019-05-01", "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01")
  )
  expect_s3_class(comparativa_meses_condatos$fecha, "Date")
})
