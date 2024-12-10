test_that("Authentification works", {
  expect_true(is.null(authenticity_check("FGJA", "UASLP")))
})
