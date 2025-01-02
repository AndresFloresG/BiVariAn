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
