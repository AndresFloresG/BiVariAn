#' @importFrom dplyr "%>%"
#' @importFrom mice mice
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr drop_na
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

continuous_2g_pair <- function(data, groupvar, flextableformat = TRUE) {
  # Convertir la variable de agrupacion en factor
  data[[groupvar]] <- as.factor(data[[groupvar]])

  # Verificar que la variable de agrupacion tiene exactamente dos niveles
  if (length(levels(data[[groupvar]])) != 2) {
    stop("The grouping variable must have exactly two levels.")
  }

  # Seleccionar variables continuas del dataframe
  variables_continuas <- colnames(data %>% select_if(is.numeric))

  # Dividir los datos por grupo
  group_levels <- levels(data[[groupvar]])
  data_group1 <- data[data[[groupvar]] == group_levels[1], ]
  data_group2 <- data[data[[groupvar]] == group_levels[2], ]

  # Verificar si hay discrepancias en las longitudes de los grupos
  has_na_discrepancy <- any(sapply(variables_continuas, function(var) {
    length(na.omit(data_group1[[var]])) != length(na.omit(data_group2[[var]]))
  }))

  if (has_na_discrepancy) {
    if (!interactive()) {
      stop("Group lengths are mismatched and function is not in interactive mode.")
    }

    cat("Group lengths are mismatched for at least one variable.\n")
    cat("Do you want to handle missing values within this function?\n")
    ANSWER <- readline(prompt = "Type Y for yes or N for no: ")

    if (toupper(ANSWER) == "N") {
      stop("Function stopped due to mismatched group lengths.")
    } else if (toupper(ANSWER) == "Y") {
      cat("Options for handling missing values:\n")
      cat('1. Remove missing values (type "del")\n')
      cat('2. Perform multiple imputation (type "imp")\n')
      cat('3. Ignore missing values (type "ign")\n')
      manlength <- readline(prompt = "Choose an option (del/imp/ign): ")

      if (tolower(manlength) == "imp") {
        m_val <- as.integer(readline(prompt = "Number of imputations. Default 5: "))
        max_iter <- as.integer(readline(prompt = "Maximum iterations. Default 2: "))
        methodimp <- readline(prompt = 'Imputation method. Default "cart": ')
        seedimp <- as.integer(readline(prompt = "Seed for imputation. Default 1234: "))

        if (is.na(m_val)) m_val <- 5
        if (is.na(max_iter)) max_iter <- 2
        if (methodimp == "") methodimp <- "cart"
        if (is.na(seedimp)) seedimp <- 1234

        impdat <- mice::mice(data, m = m_val, maxit = max_iter, method = methodimp, seed = seedimp, printFlag = FALSE)
        data <- mice::complete(impdat)

      } else if (tolower(manlength) == "del") {
        for (var in variables_continuas){

          group_levels <- levels(data[[groupvar]])
          data_group1 <- data[data[[groupvar]] == group_levels[1], ]
          data_group2 <- data[data[[groupvar]] == group_levels[2], ]

          group1 <- data_group1[[var]]
          group2 <- data_group2[[var]]

          length(group1) = length(group2)

          paired_data <- data.frame(cbind(na.omit(group1), na.omit(group2)))
        }


      } else if (tolower(manlength) == "ign") {
        # Do nothing; proceed with ignoring missing values
        group_levels <- levels(data[[groupvar]])
        data_group1 <- data[data[[groupvar]] == group_levels[1], ]
        data_group2 <- data[data[[groupvar]] == group_levels[2], ]

        group1 <- data_group1[[var]]
        group2 <- data_group2[[var]]

        length(group1) = length(group2)

        paired_data <- data.frame(cbind(group1, group2))

      } else {
        stop("Invalid input. Function stopped.")
      }
    } else {
      stop("Invalid input. Function stopped.")
    }
  }

  # Crear listas para almacenar resultados
  resultados <- list()

  # Bucle para analizar cada variable continua
  for (var in variables_continuas) {

    group_levels <- levels(data[[groupvar]])
    data_group1 <- data[data[[groupvar]] == group_levels[1], ]
    data_group2 <- data[data[[groupvar]] == group_levels[2], ]

    group1 <- data_group1[[var]]
    group2 <- data_group2[[var]]

    # Emparejar los grupos
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
      diff <- paired_data$group1 - paired_data$group2
      shapiro_res <- shapiro.test(diff)$p.value

      if (shapiro_res > 0.05) {
        t_test <- t.test(paired_data$group1, paired_data$group2, paired = TRUE)
        t_p <- t_test$p.value
        diff_means <- mean(diff)
        ci_lower <- t_test$conf.int[1]
        ci_upper <- t_test$conf.int[2]
        wilcox_p <- NA
      } else {
        wilcox_test <- wilcox.test(paired_data$group1, paired_data$group2, paired = TRUE)
        wilcox_p <- wilcox_test$p.value
        t_p <- NA
        diff_means <- mean(diff)
        ci_lower <- NA
        ci_upper <- NA
      }

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

  resultados_df <- do.call(rbind, lapply(resultados, as.data.frame))

  if (flextableformat) {
    return(rrtable::df2flextable(resultados_df, vanilla = TRUE))
  } else {
    rownames(resultados_df) <- NULL
    return(resultados_df)
  }
}


