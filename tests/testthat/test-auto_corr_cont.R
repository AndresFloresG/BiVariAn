test_that("auto_corr_cont works", {
  data(mtcars)

  cont_plots <- auto_corr_cont(data = mtcars, referencevar = "cyl")

  expect_true(is.list(cont_plots))

  expect_length(cont_plots, 10)

  expect_equal(cont_plots$disp$labels$title, "Correlaci\u00f3n entre cyl con disp")
})
