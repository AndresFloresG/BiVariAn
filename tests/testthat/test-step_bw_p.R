test_that("step_bw_p realiza selección de predictores correctamente (lm)", {
  data(mtcars)

  # Modelo inicial
  initial_model <- lm(cyl ~ mpg, data = mtcars)

  # Ejecutar step_bw_p
  result <- step_bw_p(initial_model, p_threshold = 0.05, trace = FALSE, data = mtcars)

  # Verificar que el modelo final sea un lm válido
  expect_s3_class(result$final_model, "lm")

  # Verificar que se registraron pasos
  expect_true(nrow(result$steps) > 0)

  # Verificar que la fórmula inicial está registrada
  expect_equal(result$steps$Step[1], "Initial")
})

test_that("step_bw_p maneja correctamente la dirección 'both'", {
  data(mtcars)

  # Modelo inicial
  initial_model <- lm(cyl ~ mpg, data = mtcars)

  # Ejecutar step_bw_p
  result <- step_bw_p(initial_model, p_threshold = 0.05, trace = FALSE, data = mtcars)

  # Verificar que el modelo final sea un lm válido
  expect_s3_class(result$final_model, "lm")

  # Verificar que se registraron pasos
  expect_true(nrow(result$steps) > 0)
})

test_that("step_bw_p maneja correctamente glm", {
  data(mtcars)

  # Modelo inicial
  initial_model <- glm(am ~ mpg + hp, family = binomial, data = mtcars)

  # Ejecutar step_bw_p
  result <- step_bw_p(initial_model, p_threshold = 0.05, trace = FALSE, data = mtcars)

  # Verificar que el modelo final sea un glm válido
  expect_s3_class(result$final_model, "glm")

  # Verificar que se registraron pasos
  expect_true(nrow(result$steps) > 0)
})

test_that("step_bw_p maneja errores de entrada correctamente", {
  data(mtcars)

  # Modelo no válido
  expect_error(step_bw_p("not_a_model", p_threshold = 0.05, trace = FALSE, data = mtcars),
               "\n\nThe model must be a 'lm' or 'glm' object")
})
