#' @importFrom logistf logistf
#' @importFrom fastDummies dummy_cols
#' @importFrom dplyr setdiff
#' @importFrom BiVariAn logistf_summary
#' @title Stepwise backward for logistic Firth regression with automated dummy variables conversion (with forced terms)
#' @name step_bw_firth
#' @aliases step_bw_firth
#' @description
#' Extension code to perform stepwise backward on a logistf model with categorical variables.
#' Automatically transforms predictors that are factors into dummy variables.
#'
#' @param reg_model A logistf model object.
#' @param s_lower Lower step. Not used; included for compatibility. Default = "~1".
#' @param s_upper Upper step. Not used; included for compatibility. Default = "all".
#' @param trace Logical. Print each step. Default TRUE.
#' @param steps Maximum number of elimination steps. If NULL, set to number of predictors.
#' @param p_threshold P-value threshold for elimination. Default = 0.05.
#' @param data Data frame. If NULL, extracted from reg_model.
#' @param forced Character vector of term names to always keep. Default NULL.
#'
#' @returns An object of class "step_bw" with components:
#'  - final_model: the fitted logistf model
#'  - steps: a data.frame of each elimination step
#'
#' @references Heinze G, Ploner M, Jiricka L, Steiner G. logistf: Firth’s Bias-Reduced Logistic Regression. 2023.
#' @references Efroymson MA. Multiple regression analysis. In: Ralston A, Wilf HS, editors. Mathematical methods for digital computers. New York: Wiley; 1960.
#' @references Ullmann T, Heinze G, Hafermann L, Schilhart-Wallisch C, Dunkler D, et al. (2024) Evaluating variable selection methods for multivariable regression models: A simulation study protocol. PLOS ONE 19(8): e0308543
#'
#' @examples
#' if (requireNamespace("logistf", quietly = TRUE)) {
#'   library(logistf)
#'   data <- mtcars
#'   data$am <- as.factor(data$am)
#'   regression_model <- logistf(am ~ mpg + cyl + disp, data = data)
#'   # Perform backward stepwise, forcing 'cyl' to remain
#'   stepwise <- step_bw_firth(
#'     regression_model,
#'     forced = c("cyl"),
#'     p_threshold = 0.05,
#'     trace = FALSE
#'   )
#'   final_model <- stepwise$final_model
#'   # Show steps and summary
#'   stepwise$steps
#'   summary(final_model)
#' }
#'
#' @export
step_bw_firth <- function(reg_model,
                          s_lower = "~1",
                          s_upper = "all",
                          trace = TRUE,
                          steps = NULL,
                          p_threshold = 0.05,
                          data = NULL,
                          forced = NULL) {
  # Validate model
  if (!inherits(reg_model, "logistf")) {
    stop("The model must be a 'logistf' object.")
  }
  # Retrieve data
  if (is.null(data)) {
    data <- tryCatch(eval(reg_model$model), error = function(e) NULL)
  }
  if (!is.data.frame(data)) {
    stop("Could not retrieve a valid data frame. Please provide 'data'.")
  }
  # Validate p_threshold
  if (p_threshold < 0 || p_threshold > 1) {
    stop("p_threshold must be a number between 0 and 1")
  }
  # Extract terms
  data_classes <- attr(terms(reg_model), "dataClasses")
  vars <- names(data_classes)
  yvar <- vars[1]
  preds <- dplyr::setdiff(vars, yvar)
  # Dummy-code factor predictors
  factors <- preds[sapply(preds, function(v) is.factor(data[[v]]))]
  if (length(factors) > 0) {
    dataprov <- fastDummies::dummy_cols(
      data[, vars, drop = FALSE],
      select_columns = factors,
      remove_first_dummy = TRUE,
      remove_selected_columns = TRUE
    )
  } else {
    dataprov <- data[, vars, drop = FALSE]
  }
  # Fit initial model
  new_preds <- setdiff(names(dataprov), yvar)
  formula_init <- reformulate(new_preds, response = yvar)
  fit <- logistf::logistf(formula_init, data = dataprov)
  # Initialize steps count
  if (is.null(steps)) steps <- length(attr(terms(fit), "term.labels"))
  # Validate 'forced'
  terms_current <- attr(terms(fit), "term.labels")
  forced_terms <- character(0)
  if (!is.null(forced)) {
    if (!is.character(forced)) stop("'forced' must be a character vector of term names")
    missing <- setdiff(forced, terms_current)
    if (length(missing) > 0) {
      stop("The following 'forced' terms are not in the model: ", paste(missing, collapse = ", "))
    }
    forced_terms <- forced
  }
  models <- list()
  step_idx <- 1L
  # Initial output
  if (trace) {
    cat("\nInitial model:", deparse(formula(fit)), "\n\n")
    print(summary(fit))
  }
  models[[step_idx]] <- list(change = "Initial", formula = formula(fit))
  # Backward elimination
  while (steps > 0) {
    steps <- steps - 1
    # Extract p-values based on trace flag
    if (trace) {
      pv <- summary(fit)$prob
      pv <- pv[!names(pv) %in% "(Intercept)"]
    } else {
      p_tab <- invisible(BiVariAn::logistf_summary(fit))
      pv <- stats::setNames(p_tab$p_value, rownames(p_tab))
      pv <- pv[!names(pv) %in% "(Intercept)"]
    }
    # Exclude forced terms
    candidates <- pv[!names(pv) %in% forced_terms]
    if (!any(candidates > p_threshold, na.rm = TRUE)) {
      if (trace) cat("\nNo eliminable terms above p_threshold. Stopping.\n")
      break
    }
    term_remove <- names(candidates)[which.max(candidates)]
    maxp <- max(candidates, na.rm = TRUE)
    if (trace) cat(sprintf("\nRemoving %s (p = %.4f)\n", term_remove, maxp))
    # Update terms
    terms_current <- setdiff(terms_current, term_remove)
    if (length(terms_current) == 0) {
      if (trace) cat("\nNo more terms to remove.\n")
      break
    }
    new_formula <- reformulate(terms_current, response = yvar)
    fit <- logistf::logistf(new_formula, data = dataprov)
    step_idx <- step_idx + 1L
    models[[step_idx]] <- list(change = paste("-", term_remove), formula = formula(fit))
    if (trace) {
      print(summary(fit))
    }
  }
  # Prepare output
  steps_df <- data.frame(
    Step = sapply(models, `[[`, "change"),
    Formula = sapply(models, function(x) deparse1(x$formula)),
    stringsAsFactors = FALSE
  )
  res <- list(final_model = fit, steps = steps_df)
  class(res) <- "step_bw"
  if (trace) res else invisible(res)
}
