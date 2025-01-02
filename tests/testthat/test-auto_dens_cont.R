test_that("auto_dens_cont works", {
  data(cars)

  densplots <- auto_dens_cont(data = cars)
  expect_true(is.list(densplots))

  expect_equal(densplots$speed$labels$caption, "Linea roja: Media \n Linea azul: Mediana")
  expect_equal(densplots$speed$labels$title, "Gráfica de densidades de speed")
})
