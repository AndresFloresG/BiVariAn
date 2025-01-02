#' @importFrom car leveneTest
#' @name interp_continuous_2g
#' @title Generic interpretation for difference between 2 groups
#' @usage interp_continuous_2g(data, groupvar, contvar)
#' @param data Data frame
#' @param groupvar Grouping variable. Must have exact 2 levels
#' @param contvar Continuous variable to be contrasted
#'
#' @description
#' Automatically writes a generic interpretation for the Student's t-test, Welch's t-test or Mann Whitney U-test. The wording only considers the alternative "two.sided" and the argument "paired" as "FALSE". Currently only the Spanish text is returned, more languages are planned to be added in the future.
#' @returns It returns as output in console a generic text according to certain conditions which are as follows:
#' Format 1: Student's t-test with statistical significance. Conditions: Normality of model residuals, homogeneity of variances, p-value of the t-test<0.05.
#' Format 2: Student's t-test without statistical significance. Conditions: Normality of model residuals, homogeneity of variances, p-value>0.05.
#' Format 3: Welch's t-test with statistical significance. Conditions: Normality of model residuals, no homogeneity of variances, p-value<0.05.
#' Format 4: Welch's t-test without statistical significance. Conditions: Normality of model residuals, no homogeneity of variances, p-value>0.05.
#' Format 5: Mann-Withnney U test with statistical significance. Conditions: Non-normality of model residuals, p-value<0.05, independent of homogeneity of variances.
#' Format 6: Mann-Withnney U test without statistical significance. Conditions: Non-normality of model residuals, p-value>0.05, independent of homogeneity of variances.
#'
#' @examples
#' data(mtcars)
#' data<-mtcars
#'
#' data$am <- as.factor(data$am)
#' interp_continuous_2g(mtcars, "am", "disp")
#'
#'
#' @export

interp_continuous_2g <- function(data, groupvar, contvar) {

  if(!is.data.frame(data)){
    stop("data must be a data.frame object")
  }

  if(!(groupvar %in% names(data))){
    stop(groupvar, " is not present in provided dataframe")
  }

  if(!(contvar %in% names(data))){
    stop(contvar, " is not present in provided dataframe")
  }


  # Convertir la variable de agrupacion en factor
  data[[groupvar]] <- as.factor(data[[groupvar]])

  # Verificar que la variable de agrupacion tiene dos niveles
  if (length(levels(data[[groupvar]])) != 2) {
    stop("Grouping variable must have exactly two levels")
  }

  # Filtrar datos validos
  valid_data <- data[!is.na(data[[groupvar]]) & !is.na(data[[contvar]]), ]
  group_data <- valid_data[[groupvar]]
  cont_data <- valid_data[[contvar]]

  # Ajustar modelo lineal para analisis de normalidad y homogeneidad
  lm_model <- lm(cont_data ~ group_data)

  # Prueba de normalidad en los residuos
  shapiro_res <- stats::shapiro.test(residuals(lm_model))$p.value

  # Prueba de homogeneidad de varianzas
  levene_p <- car::leveneTest(cont_data ~ group_data)$"Pr(>F)"[1]

  labelcontvar <- if (!is.null(table1::label(data[[contvar]])))
    table1::label(data[[contvar]])
  else contvar

  # Pruebas de comparacion
  t_test <- tryCatch(
    t.test(cont_data ~ group_data, var.equal = levene_p > 0.05),
    error = function(e) NULL
  )
  mann_whitney <- tryCatch(
    wilcox.test(cont_data ~ group_data),
    error = function(e) NULL
  )

  # Extraer resultados clave
  t_p_value <- if (!is.null(t_test)) t_test$p.value else NA
  mann_u_p_value <- if (!is.null(mann_whitney)) mann_whitney$p.value else NA
  diff_means <- if (!is.null(t_test)) t_test$estimate[1] - t_test$estimate[2] else NA
  ci_lower <- if (!is.null(t_test)) t_test$conf.int[1] else NA
  ci_upper <- if (!is.null(t_test)) t_test$conf.int[2] else NA

  # Seleccion del formato
  if (shapiro_res > 0.05 && levene_p > 0.05){
    if (t_p_value < 0.05){
      texto <-paste0("La prueba de T de Student mostro una diferencia estadisticamente significativa entre los grupos ",
                     paste0(levels(data[[groupvar]])[1]), " y ", paste0(levels(data[[groupvar]])[2]),  " para la variable ", labelcontvar,". ",
                     "La diferencia de medias fue de ", round(diff_means, 2), " con IC","95 [",
                     round(ci_lower, 2), ", ", round(ci_upper, 2), "]. ",
                     "El valor p de la prueba fue ", format.pval(t_p_value, digits = 3), ".")
    } else if (t_p_value > 0.05){
      texto <-paste0("La prueba de T de Student no mostro una diferencia estadisticamente significativa entre los grupos ",
                     paste0(levels(data[[groupvar]])[1]), " y ", paste0(levels(data[[groupvar]])[2]),  " para la variable ", labelcontvar,". ",
                     "La diferencia de medias fue ", round(diff_means, 2), " con IC","95 [",
                     round(ci_lower, 2), ", ", round(ci_upper, 2), "]. ",
                     "El valor p de la prueba fue ", format.pval(t_p_value, digits = 3), ".")
    }

  } else if (shapiro_res > 0.05 && levene_p < 0.05){
    if (t_p_value < 0.05){
      texto <- paste0( "La prueba de T de Welch mostro una diferencia estadisticamente significativa entre los grupos ",
                       paste0(levels(data[[groupvar]])[1]), " y ", paste0(levels(data[[groupvar]])[2]),  " para la variable ", labelcontvar,". ",
                       "La diferencia de medias fue ", round(diff_means, 2), " con IC", "95 [",
                       round(ci_lower, 2), ",", round(ci_upper, 2), "].",
                       "El valor p de la prueba fue ", format.pval(t_p_value, digits = 3), ".")
    } else if (t_p_value > 0.05){
      texto <- paste0( "La prueba de T de Welch no mostro una diferencia estadisticamente significativa entre los grupos ",
                       paste0(levels(data[[groupvar]])[1]), " y ", paste0(levels(data[[groupvar]])[2]), " para la variable ", labelcontvar,". ",
                       "La diferencia de medias fue ", round(diff_means, 2), " con IC", "95 [",
                       round(ci_lower, 2), ", ", round(ci_upper, 2), "].",
                       "El valor p de la prueba fue ", format.pval(t_p_value, digits = 3), ".")
    }

  } else if (shapiro_res < 0.05) {
    if (mann_u_p_value < 0.05){
      texto <- paste0( "La prueba de U de Mann Whitnney mostro una diferencia estadisticamente significativa entre los grupos ",
                       paste0(levels(data[[groupvar]])[1]), " y ", paste0(levels(data[[groupvar]])[2]), " para la variable ", labelcontvar,". ",
                       "La diferencia de medias fue ", round(diff_means, 2), " con IC", "95 [",
                       round(ci_lower, 2), ",", round(ci_upper, 2), "].",
                       "El valor p de la prueba fue ", format.pval(mann_u_p_value, digits = 3), ".")
    } else if (mann_u_p_value > 0.05){
      texto <- paste0( "La prueba de U de Mann Whitnney no mostro una diferencia estadisticamente significativa entre los grupos ",
                       paste0(levels(data[[groupvar]])[1]), " y ", paste0(levels(data[[groupvar]])[2]),  " para la variable ", labelcontvar,". ",
                       "La diferencia de medias fue ", round(diff_means, 2), " con IC", "95 [",
                       round(ci_lower, 2), ", ", round(ci_upper, 2), "].",
                       "El valor p de la prueba fue ", format.pval(mann_u_p_value, digits = 3), ".")
    }
  }

  # Imprimir el texto y los resultados de las pruebas
  cat("\n\n",texto, "\n\n")

  if (!is.null(t_test)) {
    cat("Resultados de la prueba T:\n")
    print(t_test)
  }

  if (!is.null(mann_whitney)) {
    cat("\nResultados de la prueba U de Mann-Whitney:\n")
    print(mann_whitney)
  }
}
