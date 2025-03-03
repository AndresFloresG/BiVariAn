#' @importFrom magrittr '%>%'
#' @importFrom dplyr select_if
#' @importFrom rrtable df2flextable
#' @importFrom DescTools SpearmanRho
#' @importFrom DescTools KendallTauB
#' @importFrom table1 label
#'
#' @name continuous_corr_test
#' @aliases continuous_corr_test
#' @description
#' Automatic correlation analyses for continuous variables with one variable as reference. Variable names can be assigned using [table1::label()] function.
#'
#' @title Bivariate analysis for correlation tests
#' @param data Data frame from which variables will be extracted.
#' @param referencevar Reference variable. Must be a continuous variable.
#' @param alternative Alternative for cor.test. Must be either "two.sided", "geater" or "less"
#' @param flextableformat Logical operator to indicate the output desired. Default is TRUE. When FALSE, function will return a dataframe format. Because the function calculates different statistics for each correlation (specially in kendall correlation test), it may take some time to run. You can select individual variables using the pipe operator and the select function to run correlations only on the selected variables.
#' @param corr_test Correlation test to be performed
#'
#' @returns A dataframe or flextable containing pvalues for correlation tests along with the normality and homocedasticity tests p values
#' @examples
#' # example code
#'

#' data <- data.frame(group = rep(letters[1:2], 15),
#' var1 = rnorm(30, mean = 15, sd = 5),
#' var2 = rnorm(30, mean = 20, sd = 2),
#' var3 = rnorm(30, mean = 10, sd = 1),
#' var4 = rnorm(30, mean = 5, sd =2))
#'
#' data$group<-as.factor(data$group)
#'
#' continuous_corr_test(data = data, referencevar = "var1", flextableformat = FALSE)
#'
#' # Set names to variables
#' if(requireNamespace("table1")){
#' table1::label(data$var2) <- "Variable 2"
#' table1::label(data$var3) <- "Variable 3"
#' table1::label(data$var4) <- "Variable 4"
#'
#' continuous_corr_test(data = data, referencevar = "var1", flextableformat = FALSE)
#' }
#'
#' # Example performing correlation test for only one variable
#'if(requireNamespace("dplyr")){
#' library(dplyr)
#' continuous_corr_test(data = data %>% select("var1","var2"),
#'  referencevar = "var1", flextableformat = FALSE, corr_test = "pearson")
#'}
#'
#' # Example performing only pearson correlation test
#' continuous_corr_test(data = data, referencevar = "var1",
#'  flextableformat = FALSE, corr_test = "pearson")
#'
#'
#'
#' @export
#'
continuous_corr_test <- function(data,
                                 referencevar,
                                 alternative = NULL,
                                 flextableformat = TRUE,
                                 corr_test = c("all", "pearson", "spearman", "kendall")) {

  # Configuración inicial de los análisis a realizar
  perform_pearson <- FALSE
  perform_spearman <- FALSE
  perform_kendall <- FALSE

  if(!(is.data.frame(data))){
    stop("data must be a data.frame object")
  }

  if(!(referencevar %in% names(data))){
    stop("referencevar is not inprovided data.frame")
  }

  valid_tests <- c("all", "pearson", "spearman", "kendall")
  valid_alternative <- c("two.sided", "less", "greater")

  if (!all(corr_test %in% valid_tests)) {
    stop("Invalid value in corr_test. Allowed values are: all, pearson, spearman, kendall")
  }

  if(!all(alternative %in% valid_alternative)){
    stop("Invalid alternative. Allowed alternatives are: two.sided, less, greater")
  }

  if ("all" %in% corr_test || is.null(corr_test)) {
    perform_pearson <- TRUE
    perform_spearman <- TRUE
    perform_kendall <- TRUE
  } else {
    if ("pearson" %in% corr_test) perform_pearson <- TRUE
    if ("spearman" %in% corr_test) perform_spearman <- TRUE
    if ("kendall" %in% corr_test) perform_kendall <- TRUE
  }

  # Identificar variables continuas
  cont_var <- colnames(data)[sapply(data, function(x) is.numeric(x) && !identical(x, data[[referencevar]]))]

  # Inicializar resultados
  results <- list()

  # Configuración del parámetro alternative
  if (is.null(alternative)) {
    alternative <- "two.sided"
  }

  # Bucle para analizar cada variable continua
  for (variable in cont_var) {
    if (variable %in% names(data)) {
      ref_var_data <- data[[referencevar]]
      continuous_data <- data[[variable]]

      variable_lab <- if(!is.null(table1::label(data[[variable]]))) table1::label(data[[variable]]) else variable

      # Verificar longitud de los datos
      if (length(ref_var_data) < 2 || length(continuous_data) < 2) {
        results[[variable]] <- list(
          Variable = variable_lab,
          P_Shapiro_Resid = NA,
          P_Pearson = if (perform_pearson) NA else NULL,
          P_Spearman = if (perform_spearman) NA else NULL,
          P_Kendall = if (perform_kendall) NA else NULL,
          r_Pearson = if (perform_pearson) NA else NULL,
          r_P_CI_L = if (perform_pearson) NA else NULL,
          r_P_CI_H = if (perform_pearson) NA else NULL,
          t_Kendall = if (perform_kendall) NA else NULL,
          t_K_CI_L = if (perform_kendall) NA else NULL,
          t_K_CI_H = if (perform_kendall) NA else NULL,
          rho_Spearman = if (perform_spearman) NA else NULL,
          rho_S_CI_L = if (perform_spearman) NA else NULL,
          rho_S_CI_H = if (perform_spearman) NA else NULL
        )
        next
      }

      # Realizar análisis de correlación según los métodos seleccionados
      tryCatch({
        lm_model <- lm(continuous_data ~ ref_var_data)
        shapiro_res <- stats::shapiro.test(residuals(lm_model))$p.value

        if (perform_pearson) {
          pearson_test <- cor.test(continuous_data, ref_var_data, method = "pearson", alternative = alternative)
          pears_p <- pearson_test$p.value
          pears_estim <- pearson_test$estimate
          pears_cilow <- pearson_test$conf.int[1]
          pears_cihi <- pearson_test$conf.int[2]
        } else {
          pears_p <- pears_estim <- pears_cilow <- pears_cihi <- NA
        }

        if (perform_spearman) {
          spearman_test <- cor.test(continuous_data, ref_var_data, method = "spearman", alternative = alternative)
          spear_p <- spearman_test$p.value
          spear_estim <- spearman_test$estimate
          spear_cilow <- DescTools::SpearmanRho(continuous_data, ref_var_data, conf.level = .95)[2]
          spear_cihi <- DescTools::SpearmanRho(continuous_data, ref_var_data, conf.level = .95)[3]
        } else {
          spear_p <- spear_estim <- spear_cilow <- spear_cihi <- NA
        }

        if (perform_kendall) {
          kendall_test <- cor.test(continuous_data, ref_var_data, method = "kendall", alternative = alternative)
          kend_p <- kendall_test$p.value
          kend_estim <- kendall_test$estimate
          kend_cilow <- DescTools::KendallTauB(continuous_data, ref_var_data, conf.level = .95)[2]
          kend_cihi <- DescTools::KendallTauB(continuous_data, ref_var_data, conf.level = .95)[3]
        } else {
          kend_p <- kend_estim <- kend_cilow <- kend_cihi <- NA
        }

        # Guardar resultados
        results[[variable]] <- list(
          Variable = variable_lab,
          P_Shapiro_Resid = ifelse(shapiro_res > 0.001, round(shapiro_res, 5), "<0.001*"),
          P_Pearson = ifelse(pears_p > 0.001, round(pears_p, 5), "<0.001*"),
          P_Spearman = ifelse(spear_p > 0.001, round(spear_p, 5), "<0.001*"),
          P_Kendall = ifelse(kend_p > 0.001, round(kend_p, 5), "<0.001*"),
          r_Pearson = pears_estim,
          r_P_CI_L = pears_cilow,
          r_P_CI_H = pears_cihi,
          t_Kendall = kend_estim,
          t_K_CI_L = kend_cilow,
          t_K_CI_H = kend_cihi,
          rho_Spearman = spear_estim,
          rho_S_CI_L = spear_cilow,
          rho_S_CI_H = spear_cihi
        )

      })
    } else {
      warning("\nVariable", variable, "is not present in database\n")
    }
  }

  # Convertir resultados a data frame
  resultados_df <- do.call(rbind, lapply(results, function(x) as.data.frame(x, stringsAsFactors = FALSE)))

  if(!perform_pearson){
    resultados_df$P_Pearson <- NULL
    resultados_df$r_P_CI_L <- NULL
    resultados_df$r_P_CI_H <- NULL
    resultados_df$r_Pearson <- NULL
  }

  if(!perform_spearman){
    resultados_df$P_Spearman <- NULL
    resultados_df$rho_Spearman <- NULL
    resultados_df$rho_S_CI_L <- NULL
    resultados_df$rho_S_CI_H <- NULL
  }

  if(!perform_kendall){
    resultados_df$P_Kendall <- NULL
    resultados_df$t_Kendall <- NULL
    resultados_df$t_K_CI_L <- NULL
    resultados_df$t_K_CI_H <- NULL
  }

  if (flextableformat == TRUE) {
    return(rrtable::df2flextable(resultados_df, vanilla = TRUE))
  } else {
    rownames(resultados_df) <- NULL
    return(resultados_df)
  }
}
