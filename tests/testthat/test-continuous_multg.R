test_that("continuous_multg works", {
  data <- iris

  data$Species<-as.factor(data$Species)

  cont_mult_res <- continuous_multg(data = data, groupvar = "Species", flextableformat = FALSE)

  expect_true(is.data.frame(cont_mult_res))
  expect_length(cont_mult_res, 6)

  expect_equal(cont_mult_res$Significant_Test[1], "Kruskal-Wallis")
  expect_equal(cont_mult_res$Significant_Test[2], "ANOVA")

  expect_equal(cont_mult_res$P_ANOVA[4], "<0.001*")
})

test_that("continuous_multg handle errors",{

  data <- subset(iris, Species == "setosa" )

  data$Species<-droplevels(data$Species)

  data_vec <- data$Sepal.Length

  expect_error(continuous_multg(data_vec, groupvar = "Species"), "data must be a data.frame object")

  expect_error(continuous_multg(data, groupvar = "Species"), "La variable de agrupacion debe tener al menos dos niveles con observaciones.")
})
