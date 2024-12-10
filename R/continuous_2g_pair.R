#' @importFrom dplyr "%>%"
#' @importFrom tidyr pivot_wider
#' @importFrom rrtable df2flextable
#' @name continuous_2g_pair
#' @aliases continuous_2g_pair
#' @title Bivariate analysis for 2 groups for paired data
#' @usage continuous_2g_pair(data, groupvar, flextableformat)
#' @description
#'   Generates a HTML table of bivariate analysis for 2 groups. (In development)
#' @param data Data frame from which variables will be extracted.
#' @param groupvar Grouping variable. Must have exactly 2 levels.
#' @param flextableformat Logical operator to indicate the output desired. Default is TRUE. When FALSE, function will return a dataframe format.
#'
#'
#'
#'

continuous_2g_pair <- function(data, groupvar, flextableformat = TRUE) {
  # Convertir la variable de agrupacion en factor
  data[[groupvar]] <- as.factor(data[[groupvar]])

  # Verificar que la variable de agrupacion tiene exactamente dos niveles
  if (length(levels(data[[groupvar]])) != 2) {
    stop("La variable de agrupacion debe tener exactamente dos niveles con observaciones.")
  }

  # Seleccionar variables continuas del dataframe
  variables_continuas <- colnames(data %>% select_if(is.numeric))

  # Crear listas para almacenar resultados
  resultados <- list()

  # Dividir los datos por grupo
  group_levels <- levels(data[[groupvar]])
  data_group1 <- data[data[[groupvar]] == group_levels[1], ]
  data_group2 <- data[data[[groupvar]] == group_levels[2], ]


  # Bucle para analizar cada variable continua
  for (var in variables_continuas) {
    # Extraer valores pareados
    group1 <- data_group1[[var]]
    group2 <- data_group2[[var]]

    # Verificar si hay suficientes datos no NA
    paired_data <- data.frame(group1, group2)

    if (nrow(paired_data) < 2) {
      resultados[[var]] <- list(
        Variable = var,
        P_Shapiro_Resid = NA,
        P_T_Paired = NA,
        P_Wilcoxon = NA,
        Diff_Means = NA,
        CI_Lower = NA,
        CI_Upper = NA
      )
      next
    }

    # Pruebas estadisticas
    tryCatch({
      # Prueba de normalidad para las diferencias
      diff <- paired_data$group1 - paired_data$group2
      shapiro_res <- shapiro.test(diff)$p.value

      if (shapiro_res > 0.05) {
        # Prueba T pareada
        t_test <- t.test(paired_data$group1, paired_data$group2, paired = TRUE)
        t_p <- t_test$p.value
        diff_means <- mean(diff)
        ci_lower <- t_test$conf.int[1]
        ci_upper <- t_test$conf.int[2]
        wilcox_p <- NA
      } else {
        # Prueba de Wilcoxon
        wilcox_test <- wilcox.test(paired_data$group1, paired_data$group2, paired = TRUE)
        wilcox_p <- wilcox_test$p.value
        t_p <- NA
        diff_means <- mean(diff)
        ci_lower <- NA
        ci_upper <- NA
      }

      # Guardar resultados
      resultados[[var]] <- list(
        Variable = var,
        P_Shapiro_Resid = ifelse(shapiro_res > 0.001, round(shapiro_res, 5), "<0.001*"),
        P_T_Paired = if (!is.na(t_p)) ifelse(t_p > 0.001, round(t_p, 5), "<0.001*") else NA,
        P_Wilcoxon = if (!is.na(wilcox_p)) ifelse(wilcox_p > 0.001, round(wilcox_p, 5), "<0.001*") else NA,
        Diff_Means = round(diff_means, 5),
        CI_Lower = if (!is.na(ci_lower)) round(ci_lower, 5) else NA,
        CI_Upper = if (!is.na(ci_upper)) round(ci_upper, 5) else NA
      )
    }, error = function(e) {
      # En caso de error, asignar NA a los resultados
      resultados[[var]] <- list(
        Variable = var,
        P_Shapiro_Resid = NA,
        P_T_Paired = NA,
        P_Wilcoxon = NA,
        Diff_Means = NA,
        CI_Lower = NA,
        CI_Upper = NA
      )
    })
  }

  # Convertir resultados a data frame
  resultados_df <- do.call(rbind, lapply(resultados, as.data.frame))

  if (flextableformat) {
    return(rrtable::df2flextable(resultados_df, vanilla = TRUE))
  } else {
    rownames(resultados_df) <- NULL
    return(resultados_df)
  }
}
