#' @importFrom magrittr '%>%'
#' @importFrom dplyr select_if
#' @importFrom DescTools SpearmanRho
#' @importFrom DescTools KendallTauB
#'
#' @name continuous_corr_test
#' @aliases continuous_corr_test
#' @description
#' Automatic correlation analyses for continuous variables with one variable as reference.
#'
#' @title Bivariate analysis for correlation tests
#' @param data Data frame from which variables will be extracted.
#' @param referencevar Reference variable. Must be a continuous variable.
#' @param alternative Alternative for cor.test. Must be either "two.sided", "geater" or "less"
#' @param flextableformat Logical operator to indicate the output desired. Default is TRUE. When FALSE, function will return a dataframe format.
#'
#' @export
#'
continuous_corr_test<-function(data, referencevar,  alternative = NULL, flextableformat = TRUE){

  cont_var <- colnames(data) [sapply(data, function(x) is.numeric(x) && !identical(x, data[[referencevar]]))]

  results <- list()

  if (any(is.null(alternative) | alternative == "two.sided")) {
    alternative="two.sided"
  }  else alternative=alternative

  for (variable in cont_var) {
    if (variable %in% names(data)){

      ref_var_data<- data[[referencevar]]
      continuous_data <- data[[variable]]

      if (length(ref_var_data) < 2 || length(continuous_data) < 2){
        results[[variable]] <- list(
          Variable = variable,
          P_Shapiro_Resid = NA,
          P_Pearson = NA,
          P_Spearman = NA,
          P_Kendall = NA,
          r_Pearson = NA,
          r_P_CI_l = NA,
          r_P_CI_H = NA,
          t_Kendall = NA,
          t_K_CI_L = NA,
          t_K_CI_H = NA,
          rho_S = NA,
          rho_S_CI_L = NA,
          rho_S_CI_H = NA
        )
        next
      }
      tryCatch({
        lm_model <- lm(continuous_data~ref_var_data)
        shapiro_res <- stats::shapiro.test(residuals(lm_model))$p.value
        pearson_test<-cor.test(continuous_data, ref_var_data, method="pearson", alternative=alternative)
        spearman_test<-cor.test(continuous_data, ref_var_data, method="spearman", alternative=alternative)
        kendall_test<-cor.test(continuous_data, ref_var_data, method="kendall", alternative=alternative)

        pears_p<-pearson_test$p.value
        pears_estim<-pearson_test$estimate
        pears_cilow<-pearson_test$conf.int[1]
        pears_cihi<-pearson_test$conf.int[2]

        spear_p<-spearman_test$p.value
        spear_estim<-spearman_test$estimate
        spear_cilow<-DescTools::SpearmanRho(continuous_data, ref_var_data, conf.level = .95)[2]
        spear_cihi<-DescTools::SpearmanRho(continuous_data, ref_var_data, conf.level = .95)[3]

        kend_p<-spearman_test$p.value
        kend_estim<-spearman_test$estimate
        kend_cilow<-DescTools::KendallTauB(continuous_data, ref_var_data, conf.level = .95)[2]
        kend_cihi<-DescTools::KendallTauB(continuous_data, ref_var_data, conf.level = .95)[3]

        results[[variable]] <- list(
          Variable = variable,
          P_Shapiro_Resid = ifelse(shapiro_res > 0.001, round(shapiro_res, 5), "<0.001*"),
          P_Pearson = ifelse(pears_p > 0.001, round(pears_p, 5), "<0.001*"),
          P_Spearman = ifelse(spear_p > 0.001, round(spear_p, 5), "<0.001*"),
          P_Kendall = ifelse(kend_p > 0.001, round(kend_p, 5), "<0.001*"),
          r_Pearson = round(pears_estim, 5),
          r_P_CI_l = round(pears_cilow, 5),
          r_P_CI_H = round(pears_cihi, 5),
          t_Kendall = round(kend_estim, 5),
          t_K_CI_L = round(kend_cilow, 5),
          t_K_CI_H = round(kend_cihi, 5),
          rho_S = round(spear_estim, 5),
          rho_S_CI_L = round(spear_cilow, 5),
          rho_S_CI_H = round(spear_cihi, 5)
        )
      })
    } else {
      cat("\nVariable", variable, "is not present in database\n")
    }
  }
  resultados_df <- do.call(rbind, lapply(results, as.data.frame))

  if (flextableformat == TRUE){
    return(rrtable::df2flextable(resultados_df, vanilla = TRUE))
  } else {
    rownames(resultados_df)<- NULL
    return(resultados_df)
  }
}
