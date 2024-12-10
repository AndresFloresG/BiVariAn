test_that("auto_bar_categ funciona correctamente", {
  # Caso 1: Datos validos con dos variables categoricas
  data <- data.frame(
    group = rep(c("A", "B"), each = 5),
    category1 = rep(c("X", "Y"), times = 5),
    category2 = c("M", "N", "M", "N", "M", "N", "M", "N", "M", "N")
  )
  data$group<-as.factor(data$group)
  data$category1<-as.factor(data$category1)
  data$category2<-as.factor(data$category2)

  # Ejecutar la funcion
  graficas <- auto_bar_categ(data, groupvar = "group", theme_func = theme_minimal)

  # Verificar que se devuelva una lista
  expect_true(is.list(graficas))

  # Verificar que las graficas sean objetos ggplot
  expect_true(all(sapply(graficas, inherits, what = "ggplot")))

  # Verificar que las variables categóricas generen graficas
  expect_equal(names(graficas), c("category1", "category2"))

  # Caso 2: Datos sin variables categoricas
  data_no_categorical <- data.frame(
    group = rep(c("A", "B"), each = 5),
    value = c(1,2,3,4,5,6,7,8,9,10)
  )


  # Ejecutar la funcion
  graficas_no_categorical <- auto_bar_categ(data_no_categorical, groupvar = "group")

  # Verificar que la lista de graficas este vacia
  expect_true(is.list(graficas_no_categorical))
  expect_equal(length(graficas_no_categorical), 0)

  # Caso 3: Argumentos personalizados para geom_bar
  bar_args_custom <- list(fill = "skyblue", alpha = 0.7)
  graficas_custom <- auto_bar_categ(data, groupvar = "group", bar_args = bar_args_custom)

  expect_equal(bar_args_custom$fill, "skyblue")
  expect_equal(bar_args_custom$alpha, 0.7)


  # Caso 4: Etiquetas personalizadas
  label(data$category1) <- "Categoria 1 Etiqueta"
  label(data$group) <- "Grupo Etiqueta"

  graficas_labels <- auto_bar_categ(data, groupvar = "group", theme_func = theme_minimal)

  # Verificar que las etiquetas personalizadas se utilicen en las graficas
  expect_true(identical(graficas_labels$category2$labels$title,"Distribucion de category2" ))
  expect_true(identical(graficas_labels$category1$labels$title,"Distribucion de Categoria 1 Etiqueta" ))


  # Caso 5: Validacion de texto de porcentajes
  graficas_percent <- auto_bar_categ(data, groupvar = "group", theme_func = theme_minimal)

})
