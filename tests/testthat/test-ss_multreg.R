test_that("ss_multreg works", {
  expect_snapshot(results <- ss_multreg(10))

  expect_equal(class(results), "ss_multreg_obj")
  expect_length(results, 3)
  expect_equal(results$ss_optimal, 150)
  expect_equal(results$ss_low, 100)
  expect_equal(results$ss_high, 200)

})

test_that("ss_multreg handle errors", {

  expect_error(ss_multreg(df = 10, prop = 2, logistic = TRUE),
               "Argument 'prop' must be a number between 0 and 1")

  expect_error(ss_multreg(df = 10, prop = .5, logistic = FALSE),
               "If prop is provided, logistic must be TRUE")
  expect_error(ss_multreg(df = 10, prop = NULL, T),
               "If logistic is TRUE, prop cannot be NULL")

})
