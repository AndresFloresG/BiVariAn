# Crear un contexto para las pruebas
test_that("auto_shapiro_raw funciona correctamente", {
  # Caso 1: Datos normales
  data <- data.frame(
    var1 = rnorm(100, mean = 50, sd = 10),
    var2 = rnorm(100, mean = 30, sd = 5),
    group = sample(c("A", "B"), 100, replace = TRUE)
  )

  # Ejecutar la función
  resultados <- auto_shapiro_raw(data, flextableformat = FALSE)
  resultados_tab <- auto_shapiro_raw(data, flextableformat = TRUE)

  # Verificar estructura del resultado
  expect_true(is.data.frame(resultados))
  expect_equal(ncol(resultados), 3) # Debe tener 3 columnas: Variable, p_shapiro, Normality
  expect_equal(nrow(resultados), 2) # Debe haber 2 variables continuas

  # Verificar valores específicos
  expect_true(all(resultados$Variable %in% c("var1", "var2")))
  expect_true(all(resultados$Normality %in% c("Normal", "Non-normal")))

  expect_error(auto_shapiro_raw(data = data, flextableformat = data), "Argument flextableformat must be a logical operator")

  expect_error(auto_shapiro_raw(resultados_tab), "Data provided must be a data.frame object")

  # Caso 2: Datos sin variables numéricas
  data_no_numeric <- data.frame(
    group = sample(c("A", "B"), 100, replace = TRUE),
    category = sample(c("X", "Y", "Z"), 100, replace = TRUE)
  )

  # Ejecutar la función
  resultados_no_numeric <- auto_shapiro_raw(data_no_numeric, flextableformat = FALSE)

  # Verificar que el resultado sea un data.frame vacío
  expect_true(!is.data.frame(resultados_no_numeric))
  expect_equal(nrow(resultados_no_numeric), NULL) # No hay variables continuas

  # Caso 3: Validación de flextable
  resultados_flextable <- auto_shapiro_raw(data, flextableformat = TRUE)

  # Verificar que se devuelva un objeto flextable
  expect_true(inherits(resultados_flextable, "flextable"))

  # Caso 4: Datos con NA
  data_with_na <- data.frame(
    var1 = c(rnorm(50), rep(NA, 50)),
    var2 = rnorm(100, mean = 30, sd = 5)
  )

  # Ejecutar la función
  resultados_with_na <- auto_shapiro_raw(data_with_na, flextableformat = FALSE)

  # Verificar que las variables se analizan correctamente ignorando NA
  expect_true(is.data.frame(resultados_with_na))
  expect_equal(nrow(resultados_with_na), 2)
  expect_true(all(resultados_with_na$Variable %in% c("var1", "var2")))
  expect_true(all(resultados_with_na$Normality %in% c("Normal", "Non-normal")))



  # Caso 5: Validación de errores
  expect_snapshot(auto_shapiro_raw(NULL), error = T)
})
