test_that("auto_bar_cont works", {
  data <- data.frame(group = rep(letters[1:2], 30),
                     var1 = rnorm(30, mean = 15, sd = 5),
                     var2 = rnorm(30, mean = 20, sd = 2),
                     var3 = rnorm(30, mean = 10, sd = 1),
                     var4 = rnorm(30, mean = 5, sd =2))

  data$group<-as.factor(data$group)

  graficas <- auto_bar_cont(data = data,
                            groupvar = "group",
                            err_bar_show = T,
                            err_bar = "se")

  expect_true(is.list(graficas))
  expect_equal(graficas$var1$labels$x, "group")
  expect_equal(class(graficas$var1), c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"))

  expect_equal(graficas$var1$labels$title, "Bar plot de var1 por group")
})

test_that("auto_bar_cont handle non default args", {

  data <- data.frame(group = rep(letters[1:2], 30),
                     var1 = rnorm(30, mean = 15, sd = 5),
                     var2 = rnorm(30, mean = 20, sd = 2),
                     var3 = rnorm(30, mean = 10, sd = 1),
                     var4 = rnorm(30, mean = 5, sd =2))

  data$group<-as.factor(data$group)

  graficas <- auto_bar_cont(data = data,
                            groupvar = "group",
                            err_bar_show = T,
                            err_bar = "se",
                            lang_labs = "EN",
                            theme_func = theme_minimal,
                            col_args = list(fill="blue",
                                            color = "red",
                                            alpha = 0.5))

  expect_true(is.list(graficas))
  expect_equal(graficas$var1$labels$title, "Bar plot of var1 by group")

  ggbuild <- ggplot_build(graficas$var1)$data[1]

  expect_equal(ggbuild[[1]][11][1,1], "blue")
  expect_equal(ggbuild[[1]][10][1,1], "red")
})

test_that("auto_bar_cont can handle errors", {
  nondfdata <- list(group = letters[1:8],
                    var1 = (1:8))
expect_error(auto_bar_cont(nondfdata, groupvar = "group"), "data must be a data.frame object")

data <- data.frame(group = rep(letters[1:2], 30),
                   var1 = rnorm(30, mean = 15, sd = 5),
                   var2 = rnorm(30, mean = 20, sd = 2),
                   var3 = rnorm(30, mean = 10, sd = 1),
                   var4 = rnorm(30, mean = 5, sd =2))
expect_error(auto_bar_cont(data, groupvar = "other"), "The grouping variable must be a column in the data frame")
expect_error(auto_bar_cont(data, groupvar = "var1"), "The grouping variable must be categorical.")

})
