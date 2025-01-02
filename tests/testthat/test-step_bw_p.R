test_that("step_bw_p realiza selección de predictores correctamente (lm)", {
  data(mtcars)

  # Modelo inicial
  initial_model <- lm(cyl ~ mpg, data = mtcars)

  # Ejecutar step_bw_p
  result <- step_bw_p(initial_model, p_threshold = 0.05, trace = FALSE, data = mtcars)

  expect_equal(class(result), "step_bw")

  expect_s3_class(result$final_model, "lm")

  expect_true(nrow(result$steps) > 0)
  expect_equal(result$steps$Step[1], "Initial")
  expect_length(result, 2)

  final_model<-result$final_model

  expect_length(final_model, 12)

  summary<-summary(final_model)
  summary$coefficients[2]

  expect_equal(round(summary$coefficients[2], digits = 5), -0.25251)
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
  initial_model <- lm(cyl ~ mpg, data = mtcars)

  expect_error(step_bw_p(initial_model, p_threshold = 5, trace = FALSE, data = mtcars),
               "p_threshold must be a number between 0 and 1")

  expect_error(step_bw_p("not_a_model", p_threshold = 0.05, trace = FALSE, data = mtcars),
               "\n\nThe model must be a 'lm' or 'glm' object")
})
