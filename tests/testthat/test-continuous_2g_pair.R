test_that("continuous_2g_pair works", {
  data <- data.frame(group = rep(letters[1:2], 100),
                     var1 = rnorm(100, mean = 15, sd = 5),
                     var2 = rnorm(100, mean = 20, sd = 2),
                     var3 = rnorm(100, mean = 10, sd = 1),
                     var4 = rnorm(100, mean = 5, sd =2))

  data$group<-as.factor(data$group)

  cont2pair<-continuous_2g_pair(data = data, groupvar = "group", flextableformat = FALSE)

  expect_true(is.data.frame(cont2pair))

  expect_length(cont2pair, 7)

  expect_equal(colnames(cont2pair)[1], "Variable")

  cont2pair<-continuous_2g_pair(data = data, groupvar = "group", flextableformat = TRUE)

  expect_error(continuous_2g_pair(data = cont2pair, groupvar = "group", flextableformat = FALSE), "data must be a data.frame object")

})
