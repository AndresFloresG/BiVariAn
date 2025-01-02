test_that("auto_pie_categ works", {
  data <- data.frame(categ = rep(c("Categ1", "Categ2"), 25),
                     var1 = rbinom(50, 2, prob = 0.3),
                     var2 = rbinom(50, 2, prob = 0.8),
                     var3 = rbinom(50, 2, prob = 0.7))
  data$categ <- as.factor(data$categ)
  data$var1 <- as.factor(data$var1)
  data$var2 <- as.factor(data$var2)
  data$var3 <- as.factor(data$var3)

  pieplot_list <- auto_pie_categ(data = data)

  expect_true(is.list(pieplot_list))
  expect_length(pieplot_list, 4)

  expect_equal(pieplot_list$categ$labels$title, "Distribuci\u00f3n de categ")
})
