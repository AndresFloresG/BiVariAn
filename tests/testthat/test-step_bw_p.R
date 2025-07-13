test_that("step_bw_p realiza selección de predictores correctamente (lm)", {
  data(mtcars)
  initial_model <- lm(cyl ~ mpg, data = mtcars)

  result <- step_bw_p(
    initial_model,
    p_threshold = 0.05,
    trace = FALSE
  )

  expect_s3_class(result, "step_bw")
  expect_s3_class(result$final_model, "lm")
  expect_true(nrow(result$steps) >= 1)
  expect_equal(result$steps$Step[1], "Initial")
  expect_length(result, 2)

  coef_val <- coef(result$final_model)["mpg"]
  names(coef_val) <- NULL
  expect_equal(round(coef_val, 5), -0.25251)
})

test_that("step_bw_p maneja correctamente glm", {
  data(mtcars)
  initial_model <- glm(am ~ mpg + hp, family = binomial, data = mtcars)

  result <- step_bw_p(
    initial_model,
    p_threshold = 0.05,
    trace = FALSE
  )

  expect_s3_class(result$final_model, "glm")
  expect_true(nrow(result$steps) >= 1)
})

test_that("step_bw_p valida entradas y lanza errores correctos", {
  data(mtcars)
  initial_model <- lm(cyl ~ mpg, data = mtcars)

  expect_error(
    step_bw_p(initial_model, p_threshold = 5),
    "'p_threshold' must be between 0 and 1"
  )
  expect_error(
    step_bw_p("not_a_model", p_threshold = 0.05),
    "'reg_model' must be of class 'lm' or 'glm'"
  )
})

test_that("step_bw_p respeta los términos forzados", {
  data(mtcars)
  initial_model <- lm(cyl ~ mpg + wt + hp, data = mtcars)

  result <- step_bw_p(
    initial_model,
    forced = c("wt"),
    p_threshold = 0.001,
    trace = FALSE
  )

  terms_final <- attr(terms(result$final_model), "term.labels")
  expect_true("wt" %in% terms_final)
})
