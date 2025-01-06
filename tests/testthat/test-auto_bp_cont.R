test_that("auto_bp_cont works",{

  data <- data.frame(group = rep(letters[1:2], 30),
                     var1 = rnorm(30, mean = 15, sd = 5),
                     var2 = rnorm(30, mean = 20, sd = 2),
                     var3 = rnorm(30, mean = 10, sd = 1),
                     var4 = rnorm(30, mean = 5, sd =2))

  data$group<-as.factor(data$group)

  graficas <- auto_bp_cont(data, groupvar = "group")

  expect_true(is.list(graficas))

  expect_true(all(sapply(graficas, inherits, what = "ggplot")))

  expect_equal(names(graficas[1:2]), c("var1", "var2"))
})

test_that("auto_bp_cont handle errors", {
  listdf <- list(group = rep(letters[1:2], 30),
                 var1 = rnorm(30, mean = 15, sd = 5),
                 var2 = rnorm(30, mean = 20, sd = 2),
                 var3 = rnorm(30, mean = 10, sd = 1),
                 var4 = rnorm(30, mean = 5, sd =2))

  expect_error(auto_bp_cont(listdf, groupvar = "group"), "data must be a data.frame object")

  data <- data.frame(group = rep(letters[1:2], 30),
                     var1 = rnorm(30, mean = 15, sd = 5),
                     var2 = rnorm(30, mean = 20, sd = 2),
                     var3 = rnorm(30, mean = 10, sd = 1),
                     var4 = rnorm(30, mean = 5, sd =2))

  expect_error(auto_bp_cont(data = data, groupvar = "var1"), "The grouping variable must be categorical.")
  expect_error(auto_bp_cont(data = data, groupvar = "oth"), "The grouping variable must be a column in the data frame.")
  expect_error(auto_bp_cont(data = data, groupvar = "group", theme_func = "tema"), "El argumento 'theme_func' debe ser una funcion de tema valida.")

})

