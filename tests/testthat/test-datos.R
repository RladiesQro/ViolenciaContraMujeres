test_that("No hay caracteres irregulares", {
  expect_true(!all(grepl("�", unique(datos_violencia$Entidad))))
  expect_true(!all(grepl("�", unique(datos_violencia$Bien_juridico_afectado))))
  expect_true(!all(grepl("�", unique(datos_violencia$Tipo))))
  expect_true(!all(grepl("�", unique(datos_violencia$Subtipo))))
  expect_true(!all(grepl("�", unique(datos_violencia$Modalidad))))
})

test_that("Integridad de entidades ne datos de violencia", {
  expect_length(unique(datos_violencia$Entidad), 32)
  expect_equal(
    sort(unique(as.character(datos_violencia$Entidad))),
    c(
      "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Ciudad de México",
      "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán de Ocampo",
      "Morelos", "México", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo",
      "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
      "Yucatán", "Zacatecas"
    )
  )
})

test_that("Integridad de datos de poligono", {
  expect_type(poligonos_mx, "list")
  expect_s3_class(poligonos_mx, "sf")
  expect_length(poligonos_mx$nombre, 32)
  expect_equal(
    as.character(sort(poligonos_mx$nombre)),
    c(
      "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua",
      "Coahuila de Zaragoza", "Colima", "Distrito Federal", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
      "México", "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro",
      "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala",
      "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas"
    )
  )
})

test_that("Integridad de datos de población", {
  expect_equal(
    as.character(sort(poblacion_inegi_2015$Entidad)),
    c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua","Ciudad de México",
      "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán de Ocampo",
      "Morelos", "México", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo",
      "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
      "Yucatán", "Zacatecas")
  )
  expect_equal(
    sum(poblacion_inegi_2015$Habitantes2015),
    119530753
  )
})
