test_that("continuous_corr_test works", {
  data <- data.frame(group = rep(letters[1:2], 50),
                     var1 = rnorm(100, mean = 15, sd = 5),
                     var2 = rnorm(100, mean = 20, sd = 2),
                     var3 = rnorm(100, mean = 10, sd = 1),
                     var4 = rnorm(100, mean = 5, sd =2))

  data$group<-as.factor(data$group)

  data_corr <- continuous_corr_test(data = data, referencevar = "var1", flextableformat = FALSE, corr_test = "spearman")

  expect_true(is.data.frame(data_corr))
  expect_length(data_corr, 6)

  data_corr <- continuous_corr_test(data = data, referencevar = "var1", flextableformat = FALSE, corr_test = c("spearman", "pearson"))

  expect_true(is.data.frame(data_corr))
  expect_length(data_corr, 10)

})

test_that("continuous_corr_test handle errors", {
  database <- list(group = "group error",
                   var1 = "variable error")

  expect_error(continuous_corr_test(data = database, referencevar = "var1", flextableformat = FALSE, corr_test = "spearman"), "data must be a data.frame object")

  data <- data.frame(group = rep(letters[1:2], 50),
                     var1 = rnorm(100, mean = 15, sd = 5),
                     var2 = rnorm(100, mean = 20, sd = 2),
                     var3 = rnorm(100, mean = 10, sd = 1),
                     var4 = rnorm(100, mean = 5, sd =2))
  expect_error(continuous_corr_test(data = data, referencevar = "varnodatabase", flextableformat = FALSE, corr_test = "spearman"), "referencevar is not inprovided data.frame")

  expect_error(continuous_corr_test(data = data, referencevar = "var1", flextableformat = FALSE, corr_test = "other_test"), "Invalid value in corr_test. Allowed values are: all, pearson, spearman, kendall")

  expect_error(continuous_corr_test(data = data, referencevar = "var1", flextableformat = FALSE, alternative = "other_alternative"), "Invalid alternative. Allowed alternatives are: two.sided, less, greater")

})
