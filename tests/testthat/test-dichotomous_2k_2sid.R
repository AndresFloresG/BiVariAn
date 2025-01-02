test_that("dichotomous_2k_2sid works", {
  df <- data.frame(
    has = c("Yes", "No", "Yes", "Yes", "No", "No", "Yes"),
    smoke = c("Yes", "No", "No", "Yes", "No", "Yes", "No"),
    gender = c("Male", "Female", "Male", "Female", "Female", "Male", "Male"))

  df$has <- as.factor(df$has)
  df$smoke <- as.factor(df$smoke)
  df$gender <- as.factor(df$gender)

  # Set a value as reference level
  df$has <- relevel(df$has, ref= "Yes")
  df$smoke <- relevel(df$smoke, ref= "Yes")
  df$gender <- relevel(df$gender, ref= "Female")

  # Apply function


  expect_snapshot(dicot_table<-dichotomous_2k_2sid(df, referencevar="has"))

  expect_equal(class(dicot_table), "flextable")


  expect_equal(dicot_table$body$dataset$P_Chi[1], "1.00")
  expect_equal(dicot_table$col_keys, c("Variable", "Chi_Squared","Min_Expected","P_Chi", "P_Fisher", "Odds_Ratio", "CI_Lower", "CI_Upper"))

  expect_equal(dicot_table$properties$layout, "fixed")

  expect_snapshot(dicot_table<-dichotomous_2k_2sid(df, referencevar="has", flextableformat = FALSE))

  expect_true(is.data.frame(dicot_table))
})
