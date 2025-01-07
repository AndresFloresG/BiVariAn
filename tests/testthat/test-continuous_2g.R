test_that("continuous_2g funciona correctamente", {
  # Caso 1: Datos válidos con dos grupos
  data <- data.frame(
    group = rep(c("A", "B"), each = 5),
    var1 = c(50, 52, 54, 56, 58, 55, 57, 59, 60, 62),  # Valores fijos para var1
    var2 = c(30, 32, 34, 36, 38, 35, 37, 39, 40, 42)   # Valores fijos para var2
  )

  resultados <- continuous_2g(data, groupvar = "group", flextableformat = FALSE)

  # Verificar que el resultado sea un data.frame
  expect_true(is.data.frame(resultados))

  # Verificar que tiene el número correcto de filas y columnas
  expect_equal(ncol(resultados), 10) # 9 columnas
  expect_equal(nrow(resultados), 2) # 2 variables continuas

  # Verificar valores específicos
  expect_true(all(resultados$Variable %in% c("var1", "var2")))
  expect_true(all(resultados$Var_Equal %in% c(TRUE, FALSE)))
  expect_true(all(resultados$P_Shapiro_Resid %in% c(0.74149, 0.74149)))

  # Caso 2: Datos insuficientes
  data_insufficient <- data.frame(
    group = rep(c("A", "B"), each = 2),
    var1 = c(50, 55, 60, 65),
    var2 = c(30, 35, 40, 45)
  )

  expect_snapshot(resultados_insufficient <- continuous_2g(data_insufficient, groupvar = "group", flextableformat = FALSE))


  # Verificar que el resultado contenga NA para todas las métricas
  expect_true(is.null(resultados_insufficient))
  expect_true(all(is.na(resultados_insufficient$P_T_Test)))

  # Caso 3: Datos con valores NA

  set.seed(29292)
  data_with_na <- data.frame(
    group = c(rep("A", 50), rep("B", 50)),
    var1 = c(rnorm(45, mean = 50, sd = 10), rep(NA, 5), rnorm(45, mean = 55, sd = 10), rep(NA, 5)),
    var2 = c(rnorm(50, mean = 30, sd = 5), rnorm(50, mean = 35, sd = 5))
  )
  rm(.Random.seed, envir = globalenv())


  resultados_with_na <- continuous_2g(data_with_na, groupvar = "group", flextableformat = FALSE)

  # Verificar que las métricas no sean NA para las variables con datos suficientes
  expect_true(is.data.frame(resultados_with_na))
  expect_equal(nrow(resultados_with_na), 2)
  expect_true(all(!is.na(resultados_with_na[resultados_with_na$Variable == "var2", ]$P_T_Test)))

})

test_that("continuous_2g handle errors", {
  # Caso 4: Sin variables continuas
  data_no_numeric <- data.frame(
    group = rep(c("A", "B"), each = 50),
    category = sample(c("X", "Y", "Z"), 100, replace = TRUE)
  )

  resultados_no_numeric <- continuous_2g(data_no_numeric, groupvar = "group", flextableformat = FALSE)

  # Verificar que el resultado sea un objeto null
  expect_true(is.null(resultados_no_numeric))

  # Caso 6: Validacion de errores
  expect_error(continuous_2g(data.frame(), groupvar = "group"), "group is not in the provided dataframe")


  data <- data.frame(
    group = rep(c("A", "B"), each = 5),
    var1 = c(50, 52, 54, 56, 58, 55, 57, 59, 60, 62),  # Valores fijos para var1
    var2 = c(30, 32, 34, 36, 38, 35, 37, 39, 40, 42)   # Valores fijos para var2
  )

  expect_warning(resultados <- continuous_2g(data,
                                             groupvar = "group",
                                             flextableformat = FALSE,
                                             ttest_args = list(var.equal = TRUE)), "\nThe argument 'var.equal' provided in 'ttest_args' will be ignored. \nThe function will determine 'var.equal' automatically based on the Levene test.")

  # Caso 5: Validacion de flextable
  resultados_flextable <- continuous_2g(data, groupvar = "group", flextableformat = TRUE)

  # Verificar que se devuelva un objeto flextable
  expect_true(inherits(resultados_flextable, "flextable"))

  expect_error(continuous_2g(data, groupvar = "group", flextableformat = "no"), "flextableformat argument must be a logical operator")

  expect_snapshot_error((res <- continuous_2g(data,
                                groupvar = "group",
                                flextableformat = TRUE,
                                ttest_args = list(paired = TRUE))))
  expect_snapshot_error((res <- continuous_2g(data,
                                              groupvar = "group",
                                              flextableformat = TRUE,
                                              ttest_args = list(alternative = "other.alt"))))
})
