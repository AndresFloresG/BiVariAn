#' @importFrom dplyr "%>%"
#' @importFrom rrtable df2flextable
#' @import riskCommunicator
#' @name continuous_2g_2sid
#' @aliases continuous_2g_2sid
#' @title Bivariate analysis for 2 groups
#' @description
#'   Generates a HTML table of bivariate analysis for 2 groups.
#' @param data Data frame from which variables will be extracted.
#' @param groupvar Grouping variable as character. Must have exactly 2 levels.
#' @param flextableformat Logical operator to indicate the output desired. Default is TRUE. When FALSE, function will return a dataframe format.
#' @returns
#' Returns a dataframe or flextable of 2 groups 2 sided Mann Whitney's U or T test, along with Shapiro-Wilk's p values and Levene's p value.
#'
#'
#' @examples

#'  data <- data.frame(group = rep(letters[1:2], 30),
#'  var1 = rnorm(30, mean = 15, sd = 5),
#'  var2 = rnorm(30, mean = 20, sd = 2),
#'  var3 = rnorm(30, mean = 10, sd = 1),
#'  var4 = rnorm(30, mean = 5, sd =2))
#'
#'  data$group<-as.factor(data$group)
#'
#'  conttable <- continuous_2g_2sid(data = data, groupvar = "group")



#' @export
continuous_2g_2sid <- function(data, groupvar, flextableformat = TRUE) {
  if(!is.data.frame(data)){
    stop("Data must be a data.frame object")
  }

  if(is.character(flextableformat) || !is.logical(flextableformat)){
    stop("flextableformat argument must be a logical operator")
  }

  if(!(groupvar %in% names(data))){
    stop(groupvar, " is not in the provided dataframe")
  }

  # Convertir la variable de agrupacion en factor
  data[[groupvar]] <- as.factor(data[[groupvar]])
  alternative<- c("two.sided")

  # Verificar que la variable de agrupacion tiene al menos dos niveles
  if (!length(levels(data[[groupvar]])) == 2) {
    stop("Grouping variable must have exactly 2 levels")
  }

  # Seleccionar variables continuas del dataframe
  variables_continuas <- colnames(data %>% select_if(is.numeric))
  resultados <- list()

  # Bucle para analisis
  for (var1 in variables_continuas) {
    if (var1 %in% names(data)) { # Verifica si la variable existe en la base de datos
      # Extraer los datos para la prueba, ignorando NA en la variable de agrupacion y la variable continua
      valid_data <- data[!is.na(data[[groupvar]]) & (data[[var1]]), ]
      group_data <- valid_data[[groupvar]]
      continuous_data <- valid_data[[var1]]

      # Continuar solo si hay suficientes datos para analisis
      if (length(unique(group_data)) < 2 || length(continuous_data) < 2) {
        resultados[[var1]] <- list(
          Variable = var1,
          P_Shapiro_Resid = NA,
          P_Levene = NA,
          P_T_Test = NA,
          Var_Equal = NA,
          P_Mann_Whitney = NA,
          Diff_Means = NA,
          CI_Lower = NA,
          CI_Upper = NA
        )
        next
      }

      # Pruebas estadísticas en bloque tryCatch para manejar errores
      tryCatch({
        # Prueba de normalidad en los residuos
        lm_model <- lm(continuous_data ~ group_data)
        shapiro_res <- stats::shapiro.test(residuals(lm_model))$p.value

        # Prueba de homogeneidad de varianzas (Levene)
        levene_p <- car::leveneTest(continuous_data ~ group_data)$"Pr(>F)"[1]

        # Decidir si usar var.equal = TRUE o FALSE en funcion de la prueba de Levene
        var_equal <- ifelse(levene_p > 0.05, TRUE, FALSE)

        # Prueba T de Student
        t_test <- t.test(continuous_data ~ group_data, var.equal = var_equal, alternative = alternative)
        t_p <- t_test$p.value
        diff_means <- t_test$estimate[1] - t_test$estimate[2]
        ci_lower <- t_test$conf.int[1]
        ci_upper <- t_test$conf.int[2]

        # Prueba U de Mann-Whitney
        mann_whitney <- wilcox.test(continuous_data ~ group_data, alternative = alternative)
        mann_u_p <- mann_whitney$p.value

        # Guardar resultados
        resultados[[var1]] <- list(
          Variable = var1,
          P_Shapiro_Resid = ifelse(shapiro_res > 0.001, round(shapiro_res, 5), "<0.001*"),
          P_Levene = ifelse(levene_p > 0.001, round(levene_p, 5), "<0.001*"),
          P_T_Test = ifelse(t_p > 0.001, round(t_p, 5), "<0.001*"),
          Var_Equal = var_equal,
          P_Mann_Whitney = ifelse(mann_u_p > 0.001, round(mann_u_p, 5), "<0.001*"),
          Diff_Means = round(diff_means, 5),
          CI_Lower = round(ci_lower, 5),
          CI_Upper = round(ci_upper, 5)
        )
      }, error = function(e) {
        # En caso de error, asignar NA a los resultados de esta variable
        resultados[[var1]] <- list(
          Variable = var1,
          P_Shapiro_Resid = NA,
          P_Levene = NA,
          P_T_Test = NA,
          Var_Equal = NA,
          P_Mann_Whitney = NA,
          Diff_Means = NA,
          CI_Lower = NA,
          CI_Upper = NA
        )
      })
    } else {
      cat("\nLa variable", var1, "no esta presente en la base de datos.\n")
    }
  }

  resultados_df <- do.call(rbind, lapply(resultados, as.data.frame))

  if (flextableformat == TRUE){
    return(rrtable::df2flextable(resultados_df, vanilla = TRUE))
  } else {
    rownames(resultados_df)<- NULL
    return(resultados_df)
  }
}

