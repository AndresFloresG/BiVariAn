test_that("Authentification works", {
  credentials <- data.frame(
    HASH = c("87b8046d2b5642388379744d7486e9e7fce7facd531bbee89d0d049fe390116b07a7076e88139d660eaca59c892b2137a0085ebb141a4c0e6ce47dca87a0bdaf"),
    ID = c("JAFG")
  )

  auth_outp <- author_check("JAFG", credentials = credentials, hash_col = "HASH")

  expect_length(auth_outp, 1)

  expect_true(is.character(auth_outp))

  expect_equal(auth_outp, "Statistical analysis is authentic")

  auth_outp_na <- author_check("None", credentials = credentials, hash_col = "HASH")
  expect_equal(auth_outp_na, "Statistical analysis is not authentic Please check with your tutor or committee")

  expect_length(auth_outp_na, 1)

  expect_true(is.character(auth_outp_na))
})

test_that("Author check handle errors", {
  credentials <- list(
    HASH = c("87b8046d2b5642388379744d7486e9e7fce7facd531bbee89d0d049fe390116b07a7076e88139d660eaca59c892b2137a0085ebb141a4c0e6ce47dca87a0bdaf"),
    ID = c("JAFG")
  )

  expect_error(author_check("JAFG", credentials = credentials, hash_col = "HASH"), "Credentials data must be a dataframe")
})
