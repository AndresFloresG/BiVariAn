# Tests for internal p-value formatting helpers

test_that(".format_pvalue handles standard cases correctly", {
  skip_if_not_installed("testthat")

  # Standard values with default settings
  expect_equal(.format_pvalue(0.049), "0.049")
  expect_equal(.format_pvalue(0.05), "0.050")
  expect_equal(.format_pvalue(0.0496), "0.050") # Rounds up
  expect_equal(.format_pvalue(0.5), "0.500")
  expect_equal(.format_pvalue(1.0), "1.000")
  expect_equal(.format_pvalue(0.0), "0.000")
})

test_that(".format_pvalue handles threshold correctly", {
  skip_if_not_installed("testthat")

  # Below default threshold (0.001)
  expect_equal(.format_pvalue(0.0005), "<0.001")
  expect_equal(.format_pvalue(8.12e-17), "<0.001")
  expect_equal(.format_pvalue(1e-200), "<0.001")

  # At threshold - should show actual value
  expect_equal(.format_pvalue(0.001), "0.001")

  # Custom threshold
  expect_equal(.format_pvalue(0.005, threshold = 0.01), "<0.01")
  expect_equal(.format_pvalue(0.015, threshold = 0.01), "0.015")
})

test_that(".format_pvalue handles different digits settings", {
  skip_if_not_installed("testthat")

  expect_equal(.format_pvalue(0.12345, digits = 2), "0.12")
  expect_equal(.format_pvalue(0.12345, digits = 4), "0.1235") # Rounds
  expect_equal(.format_pvalue(0.1, digits = 5), "0.10000")

  # Threshold formatting should match digits
  expect_equal(
    .format_pvalue(0.00005, digits = 4, threshold = 0.0001),
    "<0.0001"
  )
})

test_that(".format_pvalue handles NA and NaN", {
  skip_if_not_installed("testthat")

  expect_equal(.format_pvalue(NA), "NA")
  expect_equal(.format_pvalue(NaN), "NA")
  expect_equal(.format_pvalue(NA_real_), "NA")

  # Mixed with valid values
  expect_equal(
    .format_pvalue(c(0.05, NA, 0.001)),
    c("0.050", "NA", "0.001")
  )
})

test_that(".format_pvalue handles out-of-range values", {
  skip_if_not_installed("testthat")

  # Negative values
  expect_equal(.format_pvalue(-0.5), "-0.5")
  expect_equal(.format_pvalue(-1), "-1")

  # Values > 1
  expect_equal(.format_pvalue(1.5), "1.5")
  expect_equal(.format_pvalue(999), "999")
})

test_that(".format_pvalue handles character input", {
  skip_if_not_installed("testthat")

  expect_equal(.format_pvalue("0.05"), "0.050")
  expect_equal(.format_pvalue("0.001"), "0.001")
  expect_equal(.format_pvalue("not_a_number"), "NA")
  expect_equal(
    .format_pvalue(c("0.05", "0.001", "invalid")),
    c("0.050", "0.001", "NA")
  )
})

test_that(".format_pvalue handles vectors correctly", {
  skip_if_not_installed("testthat")

  p_values <- c(0.049, 0.0001, 0.5, NA, 1.2)
  expected <- c("0.049", "<0.001", "0.500", "NA", "1.2")
  expect_equal(.format_pvalue(p_values), expected)
})

test_that(".format_pvalue edge cases for rounding", {
  skip_if_not_installed("testthat")

  # Values that round to exactly 0.050
  expect_equal(.format_pvalue(0.0495), "0.050")
  expect_equal(.format_pvalue(0.04951), "0.050")

  # Values just below 0.001
  expect_equal(.format_pvalue(0.0009999), "<0.001")
  expect_equal(.format_pvalue(0.00095), "<0.001")
})


# Tests for .add_significance()

test_that(".add_significance handles standard cases correctly", {
  skip_if_not_installed("testthat")

  p_raw <- c(0.0001, 0.008, 0.03, 0.21)

  result <- .add_significance(p_raw)

  expect_equal(result[1], "<0.001***") # p < 0.001
  expect_equal(result[2], "0.008**") # 0.001 < p < 0.01
  expect_equal(result[3], "0.030*") # 0.01 < p < 0.05
  expect_equal(result[4], "0.210") # p > 0.05, no symbol
})

test_that(".add_significance handles NA and invalid values", {
  skip_if_not_installed("testthat")

  p_raw <- c(0.03, NA, 0.01, 1.5)

  result <- .add_significance(p_raw)

  expect_equal(result[1], "0.030*")
  expect_identical(result[2], NA_character_) # No symbol for NA
  expect_equal(result[3], "0.010**")
  expect_equal(result[4], "1.5") # No symbol for out-of-range
})

test_that(".add_significance handles custom thresholds and symbols", {
  skip_if_not_installed("testthat")

  p_raw <- c(0.001, 0.02, 0.1)

  # Custom: only one threshold at 0.05
  result <- .add_significance(p_raw,
    thresholds = 0.05,
    symbols = "+"
  )

  expect_equal(result[1], "0.001+")
  expect_equal(result[2], "0.020+")
  expect_equal(result[3], "0.100")

  # Custom: different symbols
  result2 <- .add_significance(p_raw,
    thresholds = c(0.001, 0.01, 0.05),
    symbols = c("†††", "††", "†")
  )

  expect_equal(result2[1], "0.001†††")
  expect_equal(result2[2], "0.020†")
})

test_that(".add_significance handles boundary values correctly", {
  skip_if_not_installed("testthat")


  p_raw <- c(0.001, 0.01, 0.05)

  result <- .add_significance(p_raw)

  # At threshold = no symbol (not strictly less than)
  expect_equal(result[1], "0.001**") # 0.001 < 0.01 but not < 0.001
  expect_equal(result[2], "0.010*") # 0.01 < 0.05 but not < 0.01
  expect_equal(result[3], "0.050") # 0.05 not < 0.05
})

test_that(".add_significance handles character p_raw input", {
  skip_if_not_installed("testthat")

  p_raw <- c("0.03", "0.1") # Character input

  result <- .add_significance(p_raw)

  expect_equal(result[1], "0.030*")
  expect_equal(result[2], "0.100")
})

test_that(".add_significance validates input lengths", {
  skip_if_not_installed("testthat")


  # Mismatched thresholds and symbols
  expect_error(
    .add_significance("0.05", 0.05,
      thresholds = c(0.01, 0.05),
      symbols = "*"
    ),
    "must have the same length"
  )
})

test_that(".add_significance handles negative and >1 p-values", {
  skip_if_not_installed("testthat")

  p_raw <- c(-0.5, 1.5, 0.03)

  result <- .add_significance(p_raw)

  expect_equal(result[1], "-0.5") # No symbol for negative
  expect_equal(result[2], "1.5") # No symbol for >1
  expect_equal(result[3], "0.030*") # Normal case
})

test_that(".add_significance works with unsorted thresholds", {
  skip_if_not_installed("testthat")

  p_raw <- c(0.0005, 0.008, 0.03)

  # Thresholds provided in non-standard order
  result <- .add_significance(p_raw,
    thresholds = c(0.05, 0.001, 0.01),
    symbols = c("*", "***", "**"),
    format_p = FALSE
  )

  # Should still work correctly
  expect_equal(result[1], "0.0005***")
  expect_equal(result[2], "0.008**")
  expect_equal(result[3], "0.030*")
})


# Integration test: using both functions together

test_that("Integration: .format_pvalue + .add_significance workflow", {
  skip_if_not_installed("testthat")

  # Raw p-values from statistical tests
  p_raw <- c(8.12e-17, 0.0049, 0.0496, 0.21, NA, 1.5)

  # Step 1: Format
  p_fmt <- .format_pvalue(p_raw)

  expect_equal(p_fmt, c("<0.001", "0.005", "0.050", "0.210", "NA", "1.5"))

  # Step 2: Add significance
  p_final <- .add_significance(p_raw)

  expect_equal(
    p_final,
    c("<0.001***", "0.005**", "0.050", "0.210", NA_character_, "1.5")
  )
})
