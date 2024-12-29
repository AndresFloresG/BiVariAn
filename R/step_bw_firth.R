#' @importFrom logistf logistf
#' @importFrom fastDummies dummy_cols
#' @importFrom dplyr setdiff
#' @title Stepwise backward for logistic Firth regression with automated dummy variables conversion
#' @name step_bw_firth
#' @aliases step_bw_firth
#' @description
#' Extension code to perform stepwise backward to a logistf model with categorical variables. Automatically transforms predictors of the model which are factors to dummy variables.
#'
#' @param reg_model Regression model. Must be a glm or lm model
#' @param s_lower Lower step. Names of the variables to be included at the lower step. Default is "~1" (Intercept)
#' @param s_upper Upper step. Names of the variables to be included at the upper step. Default is "all" (Includes all variables in a dataframe)
#' @param trace Trace the steps in R console. Display the output of each iteration. Default is TRUE
#' @param steps Maximum number of steps in the process. If NULL, steps will be the length of the regression model introduced.
#' @param p_threshold Treshold of p value. Default is 0.05
#' @param data Dataframe to execute the stepwise process. If NULL, data will be assigned from the regression model data.
#' @param ... Arguments passed to Anova function from the "car" package
#'
#'
#' @export

step_bw_firth <- function(reg_model,
                          s_lower = "~1",
                          s_upper = "all",
                          trace = TRUE,
                          steps = NULL,
                          p_threshold = 0.05,
                          data = NULL,
                          ...) {
  # Validar que el modelo sea de tipo logistf
  if (!inherits(reg_model, "logistf")) {
    stop("\n\nThe model must be a 'logistf' object.")
  }


  # Obtener los datos si no se proporcionan
  if (is.null(data)) {
    data <- eval(reg_model$call$data)
  }

  # Extraer términos del modelo
  terms <- names(attr(terms(reg_model), "dataClasses"))
  yvar<-terms[1]
  predictors<-dplyr::setdiff(terms, yvar)


  # Identificar variables categóricas en la base de datos original
  columnvals <- predictors[sapply(predictors, function(term) is.factor(data[[term]]))]

  # Crear la base de datos dummy
  dataprov <- fastDummies::dummy_cols(
    data %>% select(all_of(terms)),
    select_columns = columnvals,
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  )

  # Ajustar el nuevo modelo inicial usando la base de datos dummy
  new_preds<-dplyr::setdiff(names(dataprov), yvar)

  formula_initial <- reformulate(new_preds, response = yvar)
  fit <- logistf(formula_initial, data = dataprov, ...)

  if (is.null(steps)) {
    steps <- length(attr(terms(fit), "term.labels"))
  }

  # Inicializar modelos y resultados
  models <- list()
  nm <- 1
  terms_current <- attr(terms(fit), "term.labels")

  # Trazar el inicio
  if (trace) {
    cat("\n\nBeginning of the model:\n", deparse(formula(fit)), "\n\n")
    print(summary(fit))
    utils::flush.console()
  }

  # Guardar el modelo inicial
  models[[nm]] <- list(change = "Initial", formula_eval = formula(fit))

  # Selección backward
  while (steps > 0) {
    steps <- steps - 1

    # Obtener p-valores del modelo actual
    pvalues <- summary(fit)$prob
    pvalues <- pvalues[!names(pvalues) %in% "(Intercept)"]

    # Identificar el término con el mayor p-valor
    if (any(pvalues > p_threshold, na.rm = TRUE)) {
      max_p <- max(pvalues, na.rm = TRUE)
      term_index <- which.max(pvalues)
      term_to_remove <- attr(terms(fit), "term.labels")[term_index]

      if (trace) {
        cat("\n\nCandidate term to remove:", term_to_remove, "with p =", max_p, "\n\n")
      }

      if (length(term_to_remove) == 0){
        cat("\n\nNo terms to be removed\n\n")
      break
        }

      # Eliminar el término y actualizar la fórmula
      terms_current <- setdiff(terms_current, term_to_remove)

      if (length(terms_current)<1){
        cat("\n\nIt is not possible to eliminate more terms\n\n")
      break
        }

      new_formula <- reformulate(terms_current, response = all.vars(formula(fit))[1])

      # Ajustar nuevo modelo
      fit <- logistf(new_formula, data = dataprov, ...)

      # Guardar el cambio
      models[[nm + 1]] <- list(change = paste("-", term_to_remove), formula_eval = formula(fit))
      nm <- nm + 1

      # Mostrar el paso
      if (trace) {
        cat("\nStep:", paste("-", term_to_remove), "\n", deparse(formula(fit)), "\n\n")
        print(summary(fit))
        utils::flush.console()
      }
    } else {
      cat("\n\nAll p values are below the threshold\n\n")
      # Salir si no hay más términos por eliminar
      break
    }
  }

  # Tabla de resultados
    Step <- sapply(models, function(x) x$change)
    Formula <- sapply(models, function(x) deparse(x$formula_eval))
  steps_results <- data.frame(cbind(Step, Formula))

  # Retornar modelo final y resultados
  list(final_model = fit, steps = steps_results)
}
