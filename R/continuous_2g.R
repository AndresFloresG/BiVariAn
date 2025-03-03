#' @importFrom dplyr "%>%"
#' @importFrom rrtable df2flextable
#' @importFrom table1 label
#' @name continuous_2g
#' @aliases continuous_2g
#' @title Bivariate analysis for 2 groups
#' @description
#' Automatic test for continuous variables for 2 groups. Variable names can be assigned using [table1::label()] function.
#' @param data Data frame from which variables will be extracted.
#' @param groupvar Grouping variable as character. Must have exactly 2 levels.
#' @param flextableformat Logical operator to indicate the output desired. Default is TRUE. When FALSE, function will return a dataframe format.
#' @param ttest_args Arguments to be passed to `t.test()` function.
#' @param wilcox_args Arguments to be passed to `wilcox.test()` function.
#' @returns
#' Returns a dataframe or flextable of 2 groups 2 sided Mann Whitney's U or T test, along with Shapiro-Wilk's p values and Levene's p value.
#'
#'
#' @examples
#'  df <- mtcars
#'  df$am <- as.factor(df$am)
#'  continuous_2g(data = df,
#'  groupvar = "am",
#'  flextableformat = FALSE)
#'
#' # Set names to variables
#' if(requireNamespace("table1")){
#' table1::label(df$mpg) <- "Miles per gallon"
#' table1::label(df$cyl) <- "Number of cylinders"
#' table1::label(df$disp) <- "Displacement"
#' table1::label(df$hp) <- "Gross horsepower"
#' table1::label(df$drat) <- "Rear axle ratio"
#'
#' continuous_2g(data = df, groupvar = "am", flextableformat = FALSE)
#' }


#' @export
continuous_2g <- function(data,
                          groupvar,
                          ttest_args = list(),
                          wilcox_args = list(),
                          flextableformat = TRUE) {

  if (!is.data.frame(data)) {
    stop("Data must be a data.frame object")
  }

  if (is.character(flextableformat) || !is.logical(flextableformat)) {
    stop("flextableformat argument must be a logical operator")
  }

  if (!(groupvar %in% names(data))) {
    stop(groupvar, " is not in the provided dataframe")
  }

  if ("var.equal" %in% names(ttest_args)) {
    warning("\nThe argument 'var.equal' provided in 'ttest_args' will be ignored. \nThe function will determine 'var.equal' automatically based on the Levene test.")
    ttest_args$var.equal <- NULL
  }

  if(any("paired" %in% names(ttest_args) || "paired" %in% names(wilcox_args))){
    stop("\nPaired is not supported in this function\nPlease use continuous_2g_pair(data, groupvar) instead")
  }

  valid_alternative <- c("two.sided", "less", "greater")

  if(("alternative" %in% names(ttest_args)) | "alternative" %in% names(wilcox_args)){
    if(!(ttest_args$alternative %in% valid_alternative)){
      stop("Invalid alternative. Allowed alternatives are: two.sided, less, greater")
    }
  }

  # Convertir la variable de agrupación en factor
  data[[groupvar]] <- as.factor(data[[groupvar]])

  # Verificar que la variable de agrupación tiene exactamente dos niveles
  if (length(levels(data[[groupvar]])) != 2) {
    stop("Grouping variable must have exactly 2 levels")
  }

  # Seleccionar variables continuas del dataframe
  variables_continuas <- colnames(data %>% dplyr::select(where(is.numeric)))
  resultados <- list()

  # Bucle para análisis
  for (var1 in variables_continuas) {
    if (var1 %in% names(data)) {
      valid_data <- data[!is.na(data[[groupvar]]) & !is.na(data[[var1]]), ]
      groupingdata <- valid_data[[groupvar]]
      continuous_data <- valid_data[[var1]]

      variable_lab <- if(!is.null(table1::label(data[[var1]]))) table1::label(data[[var1]]) else var1

      # Continuar solo si hay suficientes datos para análisis
      if (length(unique(groupingdata)) < 2 || length(continuous_data) < 2) {
        resultados[[var1]] <- list(
          Variable = variable_lab,
          P_Shapiro_Resid = NA,
          P_Levene = NA,
          P_T_Test = NA,
          Var_Equal = NA,
          P_Mann_Whitney = NA,
          Diff_Means = NA,
          CI_Lower = NA,
          CI_Upper = NA,
          Significant_test = NA
        )
        next
      }

      tryCatch({
        # Prueba de normalidad en los residuos
        lm_model <- lm(continuous_data ~ groupingdata)
        shapiro_res <- stats::shapiro.test(residuals(lm_model))$p.value

        # Prueba de homogeneidad de varianzas (Levene)
        levene_p <- car::leveneTest(continuous_data ~ groupingdata)$"Pr(>F)"[1]

        # Configurar var.equal según la prueba de Levene
        var_equal <- levene_p > 0.05

        # Extraer grupos
        group_levels <- levels(groupingdata)
        group1 <- continuous_data[groupingdata == group_levels[1]]
        group2 <- continuous_data[groupingdata == group_levels[2]]

        # Ejecutar t-test
        ttest_args <- modifyList(ttest_args, list(x = group1, y = group2, var.equal = var_equal))
        t_test <- do.call(t.test, ttest_args)
        t_p <- t_test$p.value
        diff_means <- mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)
        ci_lower <- t_test$conf.int[1]
        ci_upper <- t_test$conf.int[2]

        # Ejecutar prueba de Mann-Whitney
        wilcox_args <- modifyList(wilcox_args, list(x = group1, y = group2))
        mann_whitney <- do.call(wilcox.test, wilcox_args)
        mann_u_p <- mann_whitney$p.value

        # Inicializar `signiftest` para evitar que quede NULL
        signiftest <- "None"

        # Determinar la prueba significativa
        if (!is.na(shapiro_res) && shapiro_res > 0.05) {
          if (!is.na(levene_p) && levene_p > 0.05 && !is.na(t_p) && t_p < 0.05) {
            signiftest <- "Student T test"
          } else if (!is.na(levene_p) && levene_p < 0.05 && !is.na(t_p) && t_p < 0.05) {
            signiftest <- "Welch T test"
          }
        } else if (!is.na(shapiro_res) && shapiro_res <= 0.05 && !is.na(mann_u_p) && mann_u_p < 0.05) {
          signiftest <- "Mann-W-U test"
        }

        # Guardar resultados
        resultados[[var1]] <- list(
          Variable = variable_lab,
          P_Shapiro_Resid = ifelse(shapiro_res > 0.001, round(shapiro_res, 5), "<0.001*"),
          P_Levene = ifelse(levene_p > 0.001, round(levene_p, 5), "<0.001*"),
          P_T_Test = ifelse(t_p > 0.001, round(t_p, 5), "<0.001*"),
          Var_Equal = var_equal,
          P_Mann_Whitney = ifelse(mann_u_p > 0.001, round(mann_u_p, 5), "<0.001*"),
          Diff_Means = round(diff_means, 5),
          CI_Lower = round(ci_lower, 5),
          CI_Upper = round(ci_upper, 5),
          Significant_test = signiftest
        )
      }, error = function(e) {
        # En caso de error, asignar NA a los resultados de esta variable
        resultados[[var1]] <- list(
          Variable = variable_lab,
          P_Shapiro_Resid = NA,
          P_Levene = NA,
          P_T_Test = NA,
          Var_Equal = NA,
          P_Mann_Whitney = NA,
          Diff_Means = NA,
          CI_Lower = NA,
          CI_Upper = NA,
          Significant_test = NA
        )
      })
    }
  }

  resultados_df <- do.call(rbind, lapply(resultados, as.data.frame))

  if (flextableformat) {
    return(rrtable::df2flextable(resultados_df, vanilla = TRUE))
  } else {
    rownames(resultados_df) <- NULL
    return(resultados_df)
  }
}


