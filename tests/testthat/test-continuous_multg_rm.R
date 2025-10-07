test_that("basic input validation works", {
  expect_error(continuous_multg_rm(1, "ID", "group"), "data must be a data.frame")
  df0 <- datasets::sleep
  expect_error(continuous_multg_rm(df0, "Idx", "group"), "Idx .* present")
  expect_error(continuous_multg_rm(df0, "ID", "tiempo"), "tiempo .* present")
})

# --- lme-only on sleep (no between) ----------------------------------------

test_that("lme only with base::sleep returns expected structure", {

  df <- datasets::sleep

  res <- continuous_multg_rm(
    data = df,
    idvar = "ID",
    withinvar = "group",
    dvs = "extra",
    method = "lme",
    flextableformat = FALSE
  )

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 1L)
  expect_true(all(c(
    "Variable","n_subjects","within_levels","between_levels",
    "p_Mauchly","p_ez","p_ez_corr","correction",
    "p_lme_within","p_lme_between","p_lme_interaction"
  ) %in% names(res)))

  expect_equal(res$within_levels, 2L)
  expect_true(is.na(res$between_levels))
  # ez columns should be NA when method = "lme"
  expect_true(is.na(res$p_ez))
  expect_true(is.na(res$p_ez_corr))
  expect_true(is.na(res$p_Mauchly))
  expect_true(is.na(res$correction))
  # lme p-value should be character (may be "<0.001*")
  expect_true(is.numeric(res$p_lme_within))
  expect_true(is.na(res$p_lme_between))
  expect_true(is.na(res$p_lme_interaction))
})

# --- ez-only on sleep (no between) -----------------------------------------

test_that("ez only with base::sleep behaves correctly", {

  df <- datasets::sleep

  res <- continuous_multg_rm(
    data = df,
    idvar = "ID",
    withinvar = "group",
    dvs = "extra",
    method = "ez",
    correction = "GG",
    flextableformat = FALSE
  )

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 1L)
  expect_equal(res$within_levels, 2L)
  # With 2 within levels, Mauchly does not apply
  expect_true(is.na(res$p_Mauchly) || is.character(res$p_Mauchly))
  # ez p-values should be character
  expect_true(is.numeric(res$p_ez))
  expect_true(is.numeric(res$p_ez_corr))
  # lme columns should be NA
  expect_true(is.na(res$p_lme_within))
  expect_true(is.na(res$p_lme_between))
  expect_true(is.na(res$p_lme_interaction))
})

# --- both methods on CO2 with between = Type and within = conc (factor) ----

test_that("both methods with base::CO2 (between = Type; within = conc)", {

  df <- datasets::CO2
  # Make 'conc' a factor to use as within factor (7 levels)
  df$conc <- factor(df$conc)

  res <- continuous_multg_rm(
    data = df,
    idvar = "Plant",
    withinvar = "conc",
    dvs = "uptake",
    betweenvar = "Type",
    method = "both",
    correction = "HF",
    flextableformat = FALSE
  )

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 1L)
  expect_equal(res$within_levels, 7L)
  expect_equal(res$between_levels, 2L)
  # ez outputs present
  expect_true(is.character(res$p_ez))
  expect_true(is.character(res$p_ez_corr))
  expect_true(all(res$correction == "HF"))
  # Mauchly may exist with k >= 3
  expect_true(is.character(res$p_Mauchly) || all(is.na(res$p_Mauchly)))
  # lme outputs present
  expect_true(is.character(res$p_lme_within))
  expect_true(is.character(res$p_lme_between))
  expect_true(is.character(res$p_lme_interaction))
})

# --- flextable output using base::sleep -------------------------------------

test_that("flextable output is returned when requested", {

  df <- datasets::sleep

  res_ft <- continuous_multg_rm(
    data = df,
    idvar = "ID",
    withinvar = "group",
    dvs = "extra",
    method = "lme",
    flextableformat = TRUE
  )

  expect_true(inherits(res_ft, "flextable"))
})
