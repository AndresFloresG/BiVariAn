test_that("step_bw_firth works", {
  data<-mtcars
  data$am<-as.factor(data$am)
  regression_model<-logistf(am~mpg+cyl+disp, data=data)
  expect_snapshot(stepwise<-step_bw_firth(regression_model, trace=FALSE))

  expect_equal(class(stepwise), "step_bw")
  expect_length(stepwise, 2)

  list_steps<-stepwise$steps
  expect_equal(class(list_steps), "data.frame")
  expect_length(list_steps, 2)
  expect_equal(list_steps$Formula[3], "am ~ mpg")

  expect_equal(rownames(list_steps), c("1", "2", "3"))

  expect_equal(class(stepwise$final_model), "logistf")

  expect_equal(names(which.max(stepwise$final_model$coefficients)), "mpg")
})

test_that("step_bw_firth handle errors", {

  data<-mtcars
  data$am<-as.factor(data$am)
  regression_model<-glm(am~mpg+cyl+disp, data=data, family = binomial)

  expect_error(stepwise<-step_bw_firth(regression_model, trace=FALSE), "The model must be a 'logistf' object.")

  regression_model<-logistf(am~mpg+cyl+disp, data=data)

  listdata<-data$hp

  expect_error(stepwise<-step_bw_firth(regression_model, p_threshold = 3, trace=FALSE), "p_threshold must be a number between 0 and 1")

  expect_error(stepwise<-step_bw_firth(regression_model, data = listdata, trace=FALSE), "Could not retrieve a valid data frame. Please provide a valid 'data' argument.")


})
