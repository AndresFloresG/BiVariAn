test_that("interp_continuous_2g works", {
  data(mtcars)
  data<-mtcars
  data$am <- as.factor(data$am)
  expect_warning(interp_continuous_2g(data = data, "am", "disp"), "cannot compute exact p-value with ties")

  expect_snapshot(interp_continuous_2g(data = data , "am", "disp"))

  expect_error(interp_continuous_2g(data = data, "am", "grup"), "grup is not present in provided dataframe")

  expect_error(interp_continuous_2g(data = theme_serene(), "am", "disp"), "data must be a data.frame object")

  expect_error(interp_continuous_2g(data = data, "var1", "disp"), "var1 is not present in provided dataframe")

  data <- iris
  data$Species <- as.factor(data$Species)

  expect_error(interp_continuous_2g(data = data, "Species", "Sepal.Length"),"Grouping variable must have exactly two levels")
})
