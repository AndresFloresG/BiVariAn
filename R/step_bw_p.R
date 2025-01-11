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
#' @param ... Arguments passed to [car::Anova()] function.
#'
#' @returns An oject class step_bw containing the final model an each step performed in backward regression. The final model can be accessed using $ operator
#'
#' @references Efroymson MA. Multiple regression analysis. In: Ralston A, Wilf HS, editors. Mathematical methods for digital computers. New York: Wiley; 1960.
#'
#'
#' @examples
#' data(mtcars)
#' regression_model<-lm(cyl~., data=mtcars)
#' stepwise<-step_bw_p(regression_model, trace=FALSE)
#'
#' final_stepwise_model<-stepwise$final_model
#'
#' summary(final_stepwise_model)
#'
#'

#' @export


step_bw_p <- function (reg_model,
                       s_lower = "~1",
                       s_upper = "all",
                       trace = TRUE,
                       steps = NULL,
                       p_threshold = 0.05,
                       data = NULL,
                       ...)
{
  if (!inherits(reg_model, c("lm", "glm"))) {
    stop("\n\nThe model must be a 'lm' or 'glm' object")
  }

  if(p_threshold < 0 || p_threshold > 1){
    stop("p_threshold must be a number between 0 and 1")
  }

  if (is.null(steps)) {
    steps <- length(attr(terms(reg_model), "term.labels"))
  }
  if (is.null(data)) {
    data <- eval(reg_model$call$data)
  }
  if (is.character(s_lower)) {
    s_lower <- terms(as.formula(s_lower), data = data)
  }
  else {
    stop("\n\ns_lower must be a string with a valid formula")
  }
  if (s_upper == "all") {
    response_var <- all.vars(formula(reg_model))[1]
    all_vars <- setdiff(names(data), response_var)
    s_upper <- as.formula(paste(response_var, "~", paste(all_vars,
                                                         collapse = "+")))
  }
  else if (is.character(s_upper)) {
    s_upper <- terms(as.formula(s_upper), data = data)
  }
  else {
    stop("\n\ns_upper must be a string with a valid formula.")
  }



  models <- list()
  fit <- reg_model
  nm <- 1
  if (trace) {
    cat("\n\nBeggining of the model:\n", deparse(formula(fit)),
        "\n\n")
    print(car::Anova(fit, ...))
    utils::flush.console()
  }

  models[[nm]] <- list(change = "Initial", formula_eval = formula(fit))
  while (steps > 0) {
    steps <- steps - 1
    coef_summary <- car::Anova(fit, ...)
    pvalues <- coef_summary[, ifelse(inherits(reg_model,
                                              "glm"), "Pr(>Chisq)", "Pr(>F)"), drop = TRUE]
    pvalues <- pvalues[!rownames(coef_summary) %in% c("Residuals",
                                                      "(Intercept)")]
    if (length(pvalues) > 0 && any(pvalues > p_threshold,
                                   na.rm = TRUE)) {
      max_p <- max(pvalues, na.rm = TRUE)
      term_index <- which.max(pvalues)
      term_to_remove <- attr(terms(fit), "term.labels")[term_index]

      if (trace) {
        cat("\n\nCandidate term to be eliminated:", term_to_remove,
            "p value =", max_p, "\n")
      }

      if (is.na(term_to_remove)) {
        if(trace){
          cat("\n\nNo terms to be removed\n\n")
        }
        break
      }
      new_terms <- setdiff(attr(terms(fit), "term.labels"),
                           term_to_remove)
      if (length(new_terms) < 1) {
        if(trace){
          cat("\n\nIt is not possible to eliminate more terms\n\n")
        }
        break
      }
      new_formula <- reformulate(new_terms, response = all.vars(formula(fit))[1])
      fit <- update(fit, new_formula)
      models[[nm + 1]] <- list(change = paste("-", term_to_remove),
                               formula_eval = formula(fit))
      nm <- nm + 1
      if (trace) {
        cat("\n\nStep: Eliminated", term_to_remove, "\n",
            deparse(formula(fit)), "\n\n")
        print(car::Anova(fit, ...))
        utils::flush.console()
      }
    }
    else {
      if(trace){
        text<-("\n\nAll p values are below the threshold\n\n")
        text
      }
      break
    }
  }
  Step <- sapply(models, function(x) x$change)
  Formula <- sapply(models, function(x) deparse(x$formula_eval))
  steps_results <- data.frame(cbind(Step, Formula))
  reslist<-list(final_model = fit, steps = steps_results)
  class(reslist)<-"step_bw"
  if(trace){
    reslist
  }else if(trace == FALSE){
    invisible(reslist)
  }

}
