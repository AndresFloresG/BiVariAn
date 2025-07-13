#' @title Summary method for logistf with no printable output
#' @aliases summary.logistf
#' @description
#' Summary method for logistf models, currently this method is only used in [BiVariAn::step_bw_firth] function.
#' @param object logistf class object
#' @param verbose logical. If TRUE, the output will be printed
#' @param ... Additional arguments
#' @returns An object class 'data.frame' showing coefficients and p_values.
#' @references Heinze G, Ploner M, Jiricka L, Steiner G. logistf: Firth’s Bias-Reduced Logistic Regression. 2023. available on: <https://CRAN.R-project.org/package=logistf>
#'
#' @examples
#' # Only use if you want a non-printable version of 'summary' for a logistfnp object.
#' if (requireNamespace("logistf")) {
#'   library(logistf)
#'   data <- mtcars
#'   data$am <- as.factor(data$am)
#'
#'   regression_model <- logistf::logistf(am ~ mpg + cyl + disp, data = data)
#'   logistf_summary(regression_model)
#' }
#'
#' @export
logistf_summary <- function(object, verbose = FALSE, ...) {
  # Verificar que el objeto es de la clase correcta
  if (!inherits(object, "logistf")) {
    stop("Object is not a 'logistf' class")
  }



  # Extraer información clave
  call <- object$call
  coefficients <- object$coefficients
  var_matrix <- object$var
  ci_lower <- object$ci.lower
  ci_upper <- object$ci.upper
  prob <- object$prob
  alpha <- object$alpha
  method_ci <- object$method.ci
  loglik <- object$loglik
  df <- object$df
  n <- object$n

  # Cálculo del estadístico Chi-cuadrado
  if (!is.null(object$modcontrol$terms.fit)) {
    var_red <- var_matrix[object$modcontrol$terms.fit, object$modcontrol$terms.fit]
    coefs <- coefficients[object$modcontrol$terms.fit]
    chi2 <- rep(0, length(coefficients))
    chi2[object$modcontrol$terms.fit] <- stats::qchisq(1 - prob[object$modcontrol$terms.fit], 1)
  } else {
    var_red <- var_matrix
    coefs <- coefficients
    chi2 <- stats::qchisq(1 - prob, 1)
  }

  # Construcción de la tabla de salida
  summary_table <- cbind(
    coef = coefficients,
    se_coef = sqrt(diag(var_matrix)),
    lower_ci = ci_lower,
    upper_ci = ci_upper,
    Chisq = chi2,
    p_value = prob,
    method = ifelse(method_ci == "Wald", 1, ifelse(method_ci == "-", 3, 2))
  )

  rownames(summary_table) <- names(coefficients)
  colnames(summary_table) <- c("Coef", "SE(Coeff)", paste0("Lower ", 1 - alpha), paste0("Upper ", 1 - alpha), "Chisq", "p_value", "Method")

  # Prueba de razón de verosimilitud
  likelihood_ratio <- -2 * (loglik["null"] - loglik["full"])
  lr_test <- list(
    LR_stat = likelihood_ratio,
    df = df,
    p_value = 1 - stats::pchisq(likelihood_ratio, df)
  )

  # Prueba de Wald
  wald_stat <- tryCatch(
    {
      t(coefs) %*% solve(var_red) %*% coefs
    },
    error = function(e) {
      message("\n La matriz de varianza-covarianza es singular. No se puede calcular la prueba de Wald. \n")
      return(NA)
    }
  )
  wald_test <- list(
    Wald_stat = wald_stat,
    df = df,
    p_value = 1 - stats::pchisq(wald_stat, df)
  )
  if (verbose) {
    # Imprimir resultados en formato legible
    cat("\nCall:\n")
    print(call)

    cat("\nTabla de coeficientes:\n")
    print(round(summary_table, 8))

    cat("\nLikelihood Ratio Test:\n")
    cat("  LR =", round(lr_test$LR_stat, 8), "on", df, "df, p =", round(lr_test$p_value, 8), "\n")

    cat("\nWald Test:\n")
    if (!is.na(wald_test$Wald_stat)) {
      cat("  Wald =", round(wald_test$Wald_stat, 8), "on", df, "df, p =", round(wald_test$p_value, 8), "\n")
    } else {
      cat("  Wald Test no se pudo calcular debido a singularidad en la matriz de varianza.\n")
    }
  } else {
    summary_table
  }
  # Retornar invisiblemente la tabla de coeficientes
  return(invisible(as.data.frame(summary_table)))
}
