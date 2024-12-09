#' @importFrom dplyr between
#' @title Sample Size Calculation for multiple regression analysis
#' @aliases ss_multreg
#' @name ss_multreg
#' @usage ss_multreg(df, prop, logistic = FALSE)
#' @description
#' Calculates the recommended sample size for a multiple regression analysis.
#'
#' @param df Degrees of freedom planned to be introduced
#' @param prop Minimum prevalence of the expected event (Required if planned regression is a logistic regression)
#' @param logistic Logical operator to indicate wether the planned regression analysis is a logistic regression or not.
#'
#' @examples
#'
#' ss_multreg(4, logistic = FALSE)
#' ss_multreg(4, prop = .6, logistic = TRUE)
#'
#' @export

ss_multreg <- function(df, prop, logistic = FALSE){
    if(logistic == TRUE){
      if (between(prop, 0, 1) == TRUE){
        ss_optimal <- (df*15)/prop
        ss_low <- (df*10)/prop
        ss_high <- (df*20)/prop

        cat("\nSample Size Calculation for a multiple logistic regression with", df, "degrees of freedom and", paste0((prop*100),"%"), "prevalence of the desired event", "\n","\nMinimum recommended sample size:", round(ss_low, 3), "\n",
            "\nRecommended sample size:", round(ss_optimal, 3), "\n",
            "\nMaximum recommended sample size:", round(ss_high, 3), "\n")

      } else {
        cat("\nArgument 'prop' must be a number between 0 and 1\n")
      }
    } else {
      ss_optimal <- (df*15)
      ss_low <- (df*10)
      ss_high <- (df*20)

      cat("\nSample Size Calculation for a multiple lineal regression with", df, "degrees of freedom\n", "\n","\nMinimum recommended sample size:", round(ss_low, 3), "\n",
          "\nRecommended sample size:", round(ss_optimal, 3), "\n",
          "\nMaximum recommended sample size:", round(ss_high, 3), "\n")
  }
}
