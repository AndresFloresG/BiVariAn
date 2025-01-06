test_that("auto_corr_cont works", {
  data(mtcars)

  cont_plots <- auto_corr_cont(data = mtcars, referencevar = "cyl")

  expect_true(is.list(cont_plots))

  expect_length(cont_plots, 10)

  expect_equal(cont_plots$disp$labels$title, "Correlaci\u00f3n entre cyl con disp")
})

test_that("auto_corr_cont handle errors", {

  data(mtcars)

  expect_error(auto_corr_cont(data = mtcars, referencevar = "data"), "referencevar is not in database")

  expect_error(auto_corr_cont(data = mtcars), "referencevar cannot be NULL")

  expect_error(auto_corr_cont(data = mtcars, referencevar = "cyl", theme_func = "tema"), "Argument 'theme_func' must be a valid function")

  mtcars$am <- factor(mtcars$am)
  expect_error(auto_corr_cont(data = mtcars, referencevar = "am"), "referencevar variable must be numerical.")

})
