#' @import epitools
#' @importFrom rrtable df2flextable
#' @author JAFG
#' @title Bivariate Chi squared and Fisher Test analysis for 2 categories.
#' @aliases dichotomous_2k_2sid
#' @name dichotomous_2k_2sid
#' @description
#' Generates a HTML table of bivariate Chi squared and Fisher Test analysis for 2 categories. Display a table arranged dataframe with Chi squared statistic, minimum expected frecuencies, Chi squared p value, Fisher Test p value, and Odds ratio with 95 confidence levels. Note that you must recode factors and level the database factors in order to compute exact p values.
#'
#'
#' @param data Data frame from which variables will be extractred
#' @param referencevar Reference variable. Must have exactly 2 levels
#' @param flextableformat Logical operator to indicate the output desired. Default is TRUE. When FALSE, function will return a dataframe format.
#'
#' @return Returns a dataframe or flextable containing statistical values for Chi squared tests or Fisher's test.
#'
#' @examples
#'   # Not run
#'
#'  # Create a sample dataframe
#'  df <- data.frame(
#'    has = c("Yes", "No", "Yes", "Yes", "No", "No", "Yes"),
#'    smoke = c("Yes", "No", "No", "Yes", "No", "Yes", "No"),
#'    gender = c("Male", "Female", "Male", "Female", "Female", "Male", "Male"))
#'
#'  df$has <- as.factor(df$has)
#'  df$smoke <- as.factor(df$smoke)
#'  df$gender <- as.factor(df$gender)
#'
#' # Set a value as reference level
#'  df$has <- relevel(df$has, ref= "Yes")
#'  df$smoke <- relevel(df$smoke, ref= "Yes")
#'  df$gender <- relevel(df$gender, ref= "Female")
#'
#'  # Apply function
#' dichotomous_2k_2sid(df, referencevar="has")
#' dichotomous_2k_2sid(df, referencevar="has", flextableformat = FALSE)

#' @export

dichotomous_2k_2sid <- function(data,
                                referencevar,
                                flextableformat = TRUE) {

  if(!is.data.frame(data)){
    stop("data must be a data.frame object")
  }

  if(!(referencevar %in% names(data))){
    stop(referencevar, " is not present in provided dataframe")
  }

  if(any(is.character(flextableformat) | !is.logical(flextableformat))){
    stop("flextableformat must be a logical operator")
  }
  # Convertir la variable de referencia en factor
  data[[referencevar]] <- as.factor(data[[referencevar]])

  # Verificar que la variable de referencia tiene al menos dos niveles
  if (length(levels(data[[referencevar]])) < 2) {
    stop("La variable de agrupacion debe tener al menos dos niveles con observaciones.")
  }

  # Seleccionar variables dicotomicas (factores con 2 niveles) distintas de referencevar
  variables <- colnames(data)[sapply(data, function(x) is.factor(x) && length(levels(x)) == 2 && !identical(x, data[[referencevar]]))]

  # Crear una lista para guardar los resultados

  resultados <- list()

  # Bucle para analisis bivariado
  for (var in variables) {
    # Crear tabla de contingencia
    tabla <- table(data[[referencevar]], data[[var]])

    # Verificar si la tabla tiene al menos dos categorias validas
    if (all(dim(tabla) > 1)) {
      # Prueba de Chi cuadrada
      chi_test <- tryCatch(
        chisq.test(tabla, simulate.p.value = FALSE),
        error = function(e) NULL
      )

      # Prueba exacta de Fisher
      fisher_test <- tryCatch(
        fisher.test(tabla),
        error = function(e) NULL
      )

      # Odds Ratio e intervalo de confianza
      or <- tryCatch(
        epitools::oddsratio(tabla, method = "wald"),
        error = function(e) list(measure = matrix(NA, nrow = 1, ncol = 3))
      )

      # Extraer OR y sus IC si es posible
      odds_ratio <- if (!is.null(or$measure)) or$measure[2, "estimate"] else NA
      ci_lower <- if (!is.null(or$measure)) or$measure[2, "lower"] else NA
      ci_upper <- if (!is.null(or$measure)) or$measure[2, "upper"] else NA

      # Guardar resultados
      resultados[[var]] <- list(
        Variable = var,
        Chi_Squared = if (!is.null(chi_test)) round(chi_test$statistic, 5) else NA,
        Min_Expected = if (!is.null(chi_test)) round(min(chi_test$expected), 5) else NA,
        P_Chi = if (!is.null(chi_test)) {
          if (chi_test$p.value > 0.001) round(chi_test$p.value, 5) else "<0.001**"
        } else NA,
        P_Fisher = if (!is.null(fisher_test)) {
          if (fisher_test$p.value > 0.001) round(fisher_test$p.value, 5) else "<0.001**"
        } else NA,
        Odds_Ratio = round(odds_ratio, 5),
        CI_Lower = round(ci_lower, 5),
        CI_Upper = round(ci_upper, 5)
      )
    } else {
      # Si la tabla no es valida, registrar NA
      resultados[[var]] <- list(
        Variable = var,
        Chi_Squared = NA,
        Min_Expected = NA,
        P_Chi = NA,
        P_Fisher = NA,
        Odds_Ratio = NA,
        CI_Lower = NA,
        CI_Upper = NA
      )
    }
  }
resultados_df <- do.call(rbind, lapply(resultados, as.data.frame))
if (flextableformat == TRUE){
  return(rrtable::df2flextable(resultados_df, vanilla = TRUE))
} else{
  rownames(resultados_df) <- NULL
  return(resultados_df)
}
}


