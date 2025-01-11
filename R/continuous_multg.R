#' @importFrom dplyr "%>%"
#' @importFrom rrtable df2flextable
#' @name continuous_multg
#' @aliases continuous_multg
#' @title Bivariate analysis for more than 2 groups
#' @description
#'   Generates a HTML table of bivariate analysis for 2 groups.
#' @param data Data frame from which variables will be extracted.
#' @param groupvar Grouping variable. Must have exactly 2 levels.
#' @param flextableformat Logical operator to indicate the output desired. Default is TRUE. When FALSE, function will return a dataframe format.
#'
#' @returns A dataframe or flextable containing pvalues for each test along with the normality and homocedasticity tests p values. An extra column will be shown indicating the recommended significant test
#'
#' @examples
#' data <- iris
#'
#' data$Species<-as.factor(data$Species)
#'
#' continuous_multg(data = data, groupvar = "Species", flextableformat = FALSE)
#'
#' @export


continuous_multg<-function(data,
                           groupvar,
                           flextableformat = TRUE){
  # Convertir la variable de agrupacion en factor
  if(!is.data.frame(data) ){
    stop("data must be a data.frame object")
  }

  if(!(groupvar %in% names(data))){
    stop(groupvar, " is not present in the provided dataframe")
  }

  if(is.character(flextableformat) || !is.logical(flextableformat)){
    stop("flextableformat must be a logical operator")
  }

  data[[groupvar]] <- as.factor(data[[groupvar]])

  # Verificar que la variable de agrupacion tiene al menos dos niveles
  if (length(levels(data[[groupvar]])) < 2) {
    stop("La variable de agrupacion debe tener al menos dos niveles con observaciones.")
  }

  # Seleccionar variables continuas del dataframe
  variables_continuas <- colnames(data %>% select_if(is.numeric))
  resultados <- list()

  # Bucle para analisis
  for (var1 in variables_continuas) {
    if (var1 %in% names(data)) { # Verifica si la variable existe en la base de datos
      # Extraer los datos para la prueba, ignorando NA
      valid_data <- data[!is.na(data[[groupvar]]) & !is.na(data[[var1]]), ]
      group_data <- valid_data[[groupvar]]
      continuous_data <- valid_data[[var1]]

      # Continuar solo si hay suficientes datos para analisis
      if (length(unique(group_data)) < 2 || length(continuous_data) < 2) {
        resultados[[var1]] <- list(
          Variable = var1,
          P_Shapiro_Resid = NA,
          P_Levene = NA,
          P_ANOVA = NA,
          P_KW = NA,
          Significant_Test = NA
        )
        next
      }

      # Pruebas estadisticas en bloque tryCatch para manejar errores
      tryCatch({
        # Prueba de normalidad en los residuos
        lm_model <- lm(continuous_data ~ group_data)
        shapiro_res <- stats::shapiro.test(residuals(lm_model))$p.value

        # Prueba de homogeneidad de varianzas (Levene)
        levene_p <- car::leveneTest(continuous_data ~ group_data)$"Pr(>F)"[1]

        if (shapiro_res > 0.05 && levene_p > 0.05) {
          # Realizar ANOVA
          anova_res <- aov(continuous_data ~ group_data, data=data)
          anova_p <- summary(anova_res)[[1]]$"Pr(>F)"[1]
          significant_test <- ifelse(anova_p < 0.05, "ANOVA", "None")
        } else {
          # Realizar Kruskal-Wallis
          kw_res <- kruskal.test(continuous_data ~ group_data)
          kw_p <- kw_res$p.value
          significant_test <- ifelse(kw_p < 0.05, "Kruskal-Wallis", "None")
        }

        # Guardar resultados
        resultados[[var1]] <- list(
          Variable = var1,
          P_Shapiro_Resid = ifelse(shapiro_res > 0.001, round(shapiro_res, 5), "<0.001*"),
          P_Levene = ifelse(levene_p > 0.001, round(levene_p, 5), "<0.001*"),
          P_ANOVA = ifelse(exists("anova_p"), ifelse(anova_p > 0.001, round(anova_p, 5), "<0.001*"), NA),
          P_KW = ifelse(exists("kw_p"), ifelse(kw_p>0.001, round(kw_p, 5), "<0.001*"), NA),
          Significant_Test = significant_test
        )
      }, error = function(e) {
        # En caso de error, asignar NA a los resultados de esta variable
        resultados[[var1]] <- list(
          Variable = var1,
          P_Shapiro_Resid = NA,
          P_Levene = NA,
          P_ANOVA = NA,
          P_KW = NA,
          Significant_Test = NA
        )
      })
    } else {
      warning("\nVariable", var1, " is not present in provided dataframe\n")
    }
  }

  # Convertir resultados a data frame
  resultados_df <- do.call(rbind, lapply(resultados, as.data.frame))

  if (flextableformat == TRUE) {
    return(rrtable::df2flextable(resultados_df, vanilla = TRUE))
  } else {
    rownames(resultados_df) <- NULL
    return(resultados_df)
  }
}
