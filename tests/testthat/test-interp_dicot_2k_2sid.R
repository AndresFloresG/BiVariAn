test_that("interp_dicot_2k_2sid works", {
  df <- data.frame(
    has = c("Yes", "No", "Yes", "Yes", "No", "No", "Yes"),
    smoke = c("Yes", "No", "No", "Yes", "No", "Yes", "No"),
    gender = c("Male", "Female", "Male", "Female", "Female", "Male", "Male"),
    three_lev_cat = c("c1", "c2", "c3", "c1", "c2", "c3", "c1"))

  df$has <- as.factor(df$has)
  df$smoke <- as.factor(df$smoke)
  df$gender <- as.factor(df$gender)
  df$three_lev_cat <- as.factor(df$three_lev_cat)

  # Set a value as reference level
  df$has <- relevel(df$has, ref= "Yes")
  df$smoke <- relevel(df$smoke, ref= "Yes")
  df$gender <- relevel(df$gender, ref= "Female")

  expect_snapshot(interp_dicot_2k_2sid(data = df,
                                       var1 = "smoke",
                                       var2 = "gender"))

  expect_error(interp_dicot_2k_2sid(data = theme_serene(),
                                    var1 = "smoke",
                                    var2 = "gender"),
               "data must be a data.frame object")

  expect_error(interp_dicot_2k_2sid(data = df,
                                    var1 = "novariable",
                                    var2 = "gender"),
               "novariable is not present in provided dataframe")

  expect_error(interp_dicot_2k_2sid(data = df,
                                    var1 = "smoke",
                                    var2 = "othervar"),
               "othervar is not present in provided dataframe")

  expect_error(interp_dicot_2k_2sid(data = df,
                                    var1 = "three_lev_cat",
                                    var2 = "smoke"),
               "three_lev_cat must have exactly two levels")

  expect_error(interp_dicot_2k_2sid(data = df,
                                    var1 = "smoke",
                                    var2 = "three_lev_cat"),
               "three_lev_cat must have exactly two levels")
})
