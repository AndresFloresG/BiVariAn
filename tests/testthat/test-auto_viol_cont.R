test_that("auto_violin_cont works", {
  data <- data.frame(group = rep(letters[1:2], 30),
                     var1 = rnorm(30, mean = 15, sd = 5),
                     var2 = rnorm(30, mean = 20, sd = 2),
                     var3 = rnorm(30, mean = 10, sd = 1),
                     var4 = rnorm(30, mean = 5, sd =2))
  data$group<-as.factor(data$group)

  violinplotlist<-auto_viol_cont(data = data, groupvar = "group")

  expect_true(is.list(violinplotlist))

  expect_equal(capture.output(violinplotlist$var1$layers[[1]])[1], "geom_violin: na.rm = FALSE, orientation = NA, quantile_gp = list(colour = NULL, linetype = 0, linewidth = NULL)")
  expect_length(violinplotlist, 4)

  expect_error(auto_viol_cont(data = data, groupvar = "group", theme_func = data), "Argument 'theme_func' must be a valid function")

})
