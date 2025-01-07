test_that("continuous_2g_pair works", {
  set.seed(140598)
  data <- data.frame(group = rep(letters[1:2], 50),
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

  rm(.Random.seed, envir = globalenv())
})

test_that("continuous_2g_pair handle errors", {
  set.seed(140598)
  data <- data.frame(group = rep(letters[1:2], 50),
                     var1 = rnorm(100, mean = 15, sd = 5),
                     var2 = rnorm(100, mean = 20, sd = 2),
                     var3 = rnorm(100, mean = 10, sd = 1),
                     var4 = rnorm(100, mean = 5, sd =2))

  data$group<-as.factor(data$group)
  datalist <- as.list(data$var1)

  expect_error(continuous_2g_pair(data = datalist,
                                  groupvar = "cyl"),
               "data must be a data.frame object")

  expect_error(continuous_2g_pair(data = data,
                                  groupvar = "var"),"var is not in provided dataframe")

  expect_error(continuous_2g_pair(data = data,
                                  groupvar = "group",
                                  flextableformat = "no"),
               "flextableformat must be a logical operator")

  expect_warning(continuous_2g_pair(data = data,
                                  groupvar = "group",
                                  flextableformat = F,
                                  ttest_args = list(paired = TRUE)),
               "\nThe argument 'paired' provided will be ignored")

  expect_snapshot_warning(continuous_2g_pair(data = data,
                                    groupvar = "group",
                                    flextableformat = F,
                                    ttest_args = list(paired = TRUE)))

  expect_snapshot_error(continuous_2g_pair(data = data,
                                    groupvar = "group",
                                    flextableformat = F,
                                    ttest_args = list(alternative = "other")))
})



