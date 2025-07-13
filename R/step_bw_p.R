#' @importFrom car Anova
#' @title Automatized stepwise backward for regression models (with forced terms)
#' @name step_bw_p
#' @aliases step_bw_p
#'
#' @param reg_model Regression model. Must be a glm or lm object
#' @param s_lower Lower step. Names of the variables to be included at the lower step. Default is "~1" (intercept only)
#' @param s_upper Upper step. Names of the variables to be included at the upper step. Default is "all" (all predictors)
#' @param trace Logical. Whether to print each step to the console. Default is TRUE.
#' @param steps Maximum number of elimination steps. If NULL, set to number of predictors.
#' @param p_threshold P-value threshold for elimination. Default is 0.05.
#' @param data Data frame for stepwise. If NULL, extracted from reg_model.
#' @param forced Character vector of predictor names to always keep. Default NULL.
#' @param ... Additional arguments passed to car::Anova().
#'
#' @returns An object of class "step_bw" containing the final model and a data.frame of steps.
#'
#' @references Efroymson MA. Multiple regression analysis. In: Ralston A, Wilf HS, editors. Mathematical methods for digital computers. New York: Wiley; 1960.
#'
#' @examples
#' data(mtcars)
#' # Fit a linear model
#' regression_model <- lm(cyl ~ ., data = mtcars)
#' # Perform backward stepwise, forcing "wt" and "hp" to remain
#' stepwise <- step_bw_p(
#'   regression_model,
#'   forced = c("wt", "hp"),
#'   trace = FALSE
#' )
#' # Extract final model and view summary
#' final_model <- stepwise$final_model
#' summary(final_model)
#'
#' @export
step_bw_p <- function(reg_model,
                      s_lower = "~1",
                      s_upper = "all",
                      trace = TRUE,
                      steps = NULL,
                      p_threshold = 0.05,
                      data = NULL,
                      forced = NULL,
                      ...) {
  # --- Validate inputs ---
  if (!inherits(reg_model, c("lm", "glm"))) {
    stop("'reg_model' must be of class 'lm' or 'glm'")
  }
  if (!is.null(p_threshold) && (p_threshold < 0 || p_threshold > 1)) {
    stop("'p_threshold' must be between 0 and 1")
  }
  # Extract data and predictors
  if (is.null(data)) data <- eval(reg_model$call$data)

  # Process s_lower
  if (is.character(s_lower)) {
    s_lower <- terms(as.formula(s_lower), data = data)
  } else {
    stop("'s_lower' must be a character string of a formula")
  }
  # Process s_upper
  if (identical(s_upper, "all")) {
    response <- all.vars(formula(reg_model))[1]
    preds <- setdiff(names(data), response)
    s_upper <- terms(as.formula(paste(response, "~", paste(preds, collapse = "+"))),
      data = data
    )
  } else if (is.character(s_upper)) {
    s_upper <- terms(as.formula(s_upper), data = data)
  } else {
    stop("'s_upper' must be 'all' or a character string of a formula")
  }

  # Initialize steps count
  if (is.null(steps)) {
    steps <- length(attr(terms(reg_model), "term.labels"))
  }

  # Validate 'forced' argument
  forced_terms <- character(0)
  if (!is.null(forced)) {
    if (!is.character(forced)) stop("'forced' must be a character vector of term names")
    all_terms <- attr(terms(reg_model), "term.labels")
    missing <- setdiff(forced, all_terms)
    if (length(missing) > 0) {
      stop("The following 'forced' terms are not in the model: ", paste(missing, collapse = ", "))
    }
    forced_terms <- forced
  }

  # Begin stepwise
  models <- list()
  fit <- reg_model
  step_idx <- 1L
  if (trace) {
    cat("\nInitial model:", deparse(formula(fit)), "\n")
    print(car::Anova(fit, ...))
    utils::flush.console()
  }
  models[[step_idx]] <- list(
    change = "Initial",
    formula = formula(fit)
  )

  # Backward elimination loop
  while (steps > 0) {
    steps <- steps - 1
    # Compute p-values for terms
    aov_tab <- car::Anova(fit, ...)
    pcol <- if (inherits(fit, "glm")) "Pr(>Chisq)" else "Pr(>F)"
    pvals <- aov_tab[, pcol, drop = TRUE]
    names(pvals) <- rownames(aov_tab)
    # Exclude intercept and residuals
    pvals <- pvals[!names(pvals) %in% c("(Intercept)", "Residuals")]

    # Identify candidates not in 'forced'
    candidates <- pvals[!names(pvals) %in% forced_terms]
    # Stop if none above threshold
    if (!(any(candidates > p_threshold, na.rm = TRUE))) {
      if (trace) cat("\nNo eliminable terms above p_threshold. Stopping.\n")
      break
    }
    # Select term with largest p-value
    term_to_remove <- names(candidates)[which.max(candidates)]
    maxp <- max(candidates, na.rm = TRUE)
    if (trace) cat(sprintf("\nRemoving %s (p = %.4f)\n", term_to_remove, maxp))

    # Update model
    current_terms <- attr(terms(fit), "term.labels")
    new_terms <- setdiff(current_terms, term_to_remove)
    if (length(new_terms) == 0) {
      if (trace) cat("\nNo more terms to remove.\n")
      break
    }
    new_formula <- reformulate(new_terms, response = all.vars(formula(fit))[1])
    fit <- update(fit, new_formula)

    # Record step
    step_idx <- step_idx + 1L
    models[[step_idx]] <- list(
      change = paste("-", term_to_remove),
      formula = formula(fit)
    )
    if (trace) {
      print(car::Anova(fit, ...))
      utils::flush.console()
    }
  }

  # Prepare output
  steps_df <- data.frame(
    Step = sapply(models, `[[`, "change"),
    Formula = sapply(models, function(x) deparse(x$formula)),
    stringsAsFactors = FALSE
  )
  result <- list(final_model = fit, steps = steps_df)
  class(result) <- "step_bw"

  if (trace) {
    return(result)
  } else {
    invisible(result)
  }
}
