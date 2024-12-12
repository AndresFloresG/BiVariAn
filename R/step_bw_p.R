#' @importFrom car Anova
#' @title Automatized stepwise backward for regression models
#' @name step_bw_p
#' @aliases step_bw_p
#'
#' @param reg_model Regression model. Must be a glm or lm model
#' @param s_lower Lower step. Names of the variables to be included at the lower step. Default is "~1" (Intercept)
#' @param s_upper Upper step. Names of the variables to be included at the upper step. Default is "all" (Includes all variables in a dataframe)
#' @param trace Trace the steps in R console. Display the output of each iteration. Default is TRUE
#' @param steps Maximum number of steps in the process. If NULL, steps will be the length of the regression model introduced.
#' @param p_threshold Treshold of p value. Default is 0.05
#' @param data Dataframe to execute the stepwise process. If NULL, data will be assigned from the regression model data.
#' @param ... Arguments passed to or from other methods
#'
#' @examples
#' data(mtcars)
#' regression_model<-lm(cyl~., data=mtcars)
#' step_bw_p(regression_model, trace=FALSE)
#'
#'

#' @export


step_bw_p <- function(reg_model, s_lower = "~1", s_upper = "all", trace = TRUE, steps = NULL, p_threshold = 0.05, data = NULL, ...) {

  if(is.null(steps)){
    steps <- length(reg_model)
  } else {
    steps <- steps
  }

  if(is.null(data)){
    data <-eval(reg_model$call$data)
  } else{
    data <- data
  }


  # Validar que reg_model sea un modelo de regresion
  if (!inherits(reg_model, c("lm", "glm"))) {
    stop("\n\nThe model must be an 'lm' or 'glm' object")
  }

  # Procesar s_lower
  if (is.character(s_lower)) {
    s_lower <- terms(as.formula(s_lower), data = data)
  } else {
    stop("\n\ns_lower must be a string with a valid formula")
  }

  # Procesar s_upper
  if (s_upper == "all") {
    response_var <- all.vars(formula(reg_model))[1] # Variable de respuesta
    all_vars <- setdiff(names(data), response_var)   # Excluir variable de respuesta
    s_upper <- as.formula(paste(response_var, "~", paste(all_vars, collapse = "+")))
  } else if (is.character(s_upper)) {
    s_upper <- terms(as.formula(s_upper), data = data)
  } else {
    stop("\n\ns_upper must be a string with a valid formula.")
  }

  # Inicializar resultados
  models <- list()
  fit <- reg_model
  nm <- 1

  # Trazar el inicio
  if (trace) {
    cat("\n\nBeggining of the model:\n", deparse(formula(fit)), "\n\n")
    print(Anova(fit))
    utils::flush.console()
  }


  # Guardar el modelo inicial
  models[[nm]] <- list(change = "\n\nInitial", formula = formula(fit))

  while (steps > 0) {
    steps <- steps - 1

    #
    # Nota, agregar argumento para seleccionar predictores basados en valor de p de summary o Anova de car
    #


    # Obtener coeficientes y p-valores
    coef_summary <- car::Anova(fit)
    pvalues <- coef_summary[, ifelse(inherits(reg_model, "glm"), 'Pr(>Chisq)', 'Pr(>F)'), drop = TRUE]

    # Excluir el intercepto
    pvalues <- pvalues[rownames(coef_summary) != "Residuals"]

    if (length(pvalues) > 0 && any(pvalues > p_threshold, na.rm = TRUE)) {
      max_p <- max(pvalues, na.rm = TRUE)

      # Identificar posicion del termino con el mayor p-valor
      term_index <- which.max(pvalues)

      # Extraer el termino correspondiente
      term_to_remove <- attr(terms(fit), "term.labels")[term_index]

      if (trace) {
        cat("\n\nCandidate term to be eliminated:", term_to_remove, "p value =", max_p, "\n")
      }


      if(is.na(term_to_remove)){
        stop("\n\nNo terms to be removed")
      }



      # Construir nueva formula eliminando el termino
      new_terms <- setdiff(attr(terms(fit), "term.labels"), term_to_remove)

      if(length(new_terms)<1){
        stop("\n\nNo terms to be removed")
      }

      new_formula <- reformulate(new_terms, response = all.vars(formula(fit))[1])
      fit <- update(fit, new_formula)

      # Registrar el cambio
      models[[nm + 1]] <- list(change = paste("-", term_to_remove), formula_eval = formula(fit))
      nm <- nm + 1

      # Mostrar el paso
      if (trace) {
        cat("\n\nStep: Eliminated", term_to_remove, "\n", deparse(formula(fit)), "\n\n")
        print(car::Anova(fit))
        utils::flush.console()
      }
    } else {
      # No hay terminos que eliminar
      break
    }
  }

  # Tabla de resultados
  Step <- sapply(models, function(x) x$change)
  Formula <- sapply(models, function(x) deparse(x$formula_eval))

  steps_results <- data.frame(Step, Formula)

  list(final_model = fit, steps = steps_results)
}
