test_that("encode_factors convierte columnas correctamente según el tipo", {
  # Crear un data.frame de prueba
  df <- data.frame(
    char_col = c("A", "B", "C"),
    int_col = as.integer(c(1, 2, 3)),
    num_col = c(1.1, 2.2, 3.3),
    stringsAsFactors = FALSE
  )

  # Probar conversión de columnas character en factores
  result <- encode_factors(df, encode = "character")
  expect_true(is.factor(result$char_col))
  expect_false(is.factor(result$int_col))
  expect_false(is.factor(result$num_col))

  # Probar conversión de columnas integer en factores
  result <- encode_factors(df, encode = "integer")
  expect_false(is.factor(result$char_col))
  expect_true(is.factor(result$int_col))
  expect_false(is.factor(result$num_col))
})

test_that("encode_factors convierte correctamente columnas de una lista personalizada", {
  # Crear un data.frame de prueba
  df <- data.frame(
    char_col = c("A", "B", "C"),
    int_col = c(1, 2, 3),
    num_col = c(1.1, 2.2, 3.3),
    stringsAsFactors = FALSE
  )

  # Probar conversión de columnas especificadas en la lista
  result <- encode_factors(df, list_factors = c("char_col", "int_col"), uselist = TRUE)
  expect_true(is.factor(result$char_col))
  expect_true(is.factor(result$int_col))
  expect_false(is.factor(result$num_col))
})

test_that("encode_factors lanza errores con argumentos incorrectos", {
  # Crear un data.frame de prueba
  df <- data.frame(
    char_col = c("A", "B", "C"),
    int_col = c(1, 2, 3),
    num_col = c(1.1, 2.2, 3.3),
    stringsAsFactors = FALSE
  )

  # Probar error cuando data no es un data.frame
  expect_error(encode_factors("no_dataframe"), "The 'data' argument must be a data frame")

  # Probar error cuando uselist es TRUE pero list_factors es NULL
  expect_error(encode_factors(df, uselist = TRUE), "If 'uselist' is TRUE, 'list_factors' cannot be NULL")

  # Probar error cuando s_lower o s_upper no son válidos
  expect_error(encode_factors(df, encode = "invalid"), "The 'encode' argument must be either 'character' or 'integer'")
})

test_that("encode_factors no modifica columnas no especificadas", {
  # Crear un data.frame de prueba
  df <- data.frame(
    char_col = c("A", "B", "C"),
    int_col = c(1, 2, 3),
    num_col = c(1.1, 2.2, 3.3),
    stringsAsFactors = FALSE
  )

  # Convertir solo columnas character en factores
  result <- encode_factors(df, encode = "character")
  expect_false(is.factor(result$int_col))
  expect_false(is.factor(result$num_col))

  # Convertir solo columnas integer en factores
  result <- encode_factors(df, encode = "integer")
  expect_false(is.factor(result$char_col))
  expect_false(is.factor(result$num_col))
})
