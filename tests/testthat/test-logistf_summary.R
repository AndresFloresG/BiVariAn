test_that("logistf_summary errors for non-logistf objects", {
  df <- data.frame(x = 1:10, y = rbinom(10, 1, 0.5))
  expect_error(
    BiVariAn::logistf_summary(lm(y ~ x, data = df)),
    "Object is not a 'logistf' class"
  )
})

test_that("logistf_summary returns correct table for a simple model", {
  data(mtcars)
  mtcars$am <- as.factor(mtcars$am)
  mod <- logistf(am ~ mpg, data = mtcars)

  sumtab <- BiVariAn::logistf_summary(mod, verbose = FALSE)
  expect_s3_class(sumtab, "data.frame")

  # One row per coefficient
  expect_equal(nrow(sumtab), length(coef(mod)))

  # Names of columns
  expected_cols <- c(
    "Coef", "SE(Coeff)",
    paste0("Lower ", 1 - mod$alpha),
    paste0("Upper ", 1 - mod$alpha),
    "Chisq", "p_value", "Method"
  )
  expect_named(sumtab, expected_cols)

  # Coefs match
  expect_equal(as.numeric(sumtab$Coef), unname(coef(mod)))
})

test_that("logistf_summary verbose output contains all sections", {
  data(mtcars)
  mtcars$am <- as.factor(mtcars$am)
  mod <- logistf(am ~ mpg, data = mtcars)

  out <- capture.output(BiVariAn::logistf_summary(mod, verbose = TRUE))
  expect_true(any(grepl("^Call:", out)))
  expect_true(any(grepl("Tabla de coeficientes", out)))
  expect_true(any(grepl("Likelihood Ratio Test", out)))
  expect_true(any(grepl("Wald", out)))
})

test_that("logistf_summary handles singular variance-covariance matrix", {
  data(mtcars)
  mtcars$am <- as.factor(mtcars$am)
  mod <- logistf(am ~ mpg + cyl, data = mtcars)

  # Force all terms into modcontrol and zero out var matrix
  obj <- mod
  terms_len <- length(coef(mod))
  obj$modcontrol$terms.fit <- seq_len(terms_len)
  obj$var[obj$modcontrol$terms.fit, obj$modcontrol$terms.fit] <- 0

  # Expect message about singularity and still return a data.frame
  expect_message(
    res <- BiVariAn::logistf_summary(obj, verbose = FALSE),
    "matriz de varianza-covarianza es singular"
  )
  expect_s3_class(res, "data.frame")
  expect_true(all(c("Coef", "Chisq", "p_value") %in% names(res)))
})
