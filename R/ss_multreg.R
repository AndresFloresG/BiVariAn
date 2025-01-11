#' @importFrom dplyr between
#' @title Sample Size Calculation for multiple regression analysis
#' @aliases ss_multreg
#' @name ss_multreg
#' @description
#' Calculates the recommended sample size for a multiple regression analysis.
#'
#' @param df Degrees of freedom planned to be introduced
#' @param prop Minimum prevalence of the expected event (Required if planned regression is a logistic regression)
#' @param logistic Logical operator to indicate wether the planned regression analysis is a logistic regression or not.
#' @param verbose Logical operator to indicate wether the results should be printed in console. Default is `TRUE`
#'
#' @returns An object class `ss_multreg_obj` indicating the sample size calculation for a regression analysis.
#'
#' @references Peduzzi P, Concato J, Kemper E, Holford TR, Feinstein AR. A simulation study of the number of events per variable in logistic regression analysis. Journal of Clinical Epidemiology. diciembre de 1996;49(12):1373–9.
#' @references Pierdant-Pérez M, Patiño-López MI, Flores-García JA, Jacques-García FA. Implementación de un curso virtual de lectura crítica en estudiantes de medicina durante la pandemia COVID-19. Inv Ed Med. el 1 de octubre de 2023;12(48):64–71.


#'
#' @examples
#'
#' # Lineal multiple regression with 4 degrees of freedom
#' ss_multreg(4, logistic = FALSE)
#'
#' # Logistic multiple regression with 4 degrees of freedom
#' # and 60% of probability of the event
#'
#' ss_multreg(4, prop = .6, logistic = TRUE)
#'
#' @export

ss_multreg <- function(df, prop = NULL, logistic = FALSE, verbose = TRUE) {

  # Crear una lista para almacenar las salidas de texto
  text_outp <- list(
    main = NULL,
    text_min = NULL,
    text_opt = NULL,
    text_max = NULL
  )

  if(is.null(prop) && logistic == TRUE){
    stop("If logistic is TRUE, prop cannot be NULL")
  }

  if (!is.null(prop) && logistic == FALSE){
    stop("If prop is provided, logistic must be TRUE")
  }
  # Calcular tamaños de muestra para regresión logística
  if (logistic) {
    if (prop > 0 && prop <= 1) { # Validación de prop entre 0 y 1
      ss_optimal <- (df * 15) / prop
      ss_low <- (df * 10) / prop
      ss_high <- (df * 20) / prop

      # Almacenar texto en la lista
      text_outp$main <- paste("\nSample Size Calculation for a multiple logistic regression with", df,
                              "degrees of freedom and", paste0((prop * 100), "%"),
                              "prevalence of the desired event")
      text_outp$text_min <- paste("\nMinimum recommended sample size:", round(ss_low, 3), "\n")
      text_outp$text_opt <- paste("Recommended sample size:", round(ss_optimal, 3), "\n")
      text_outp$text_max <- paste("Maximum recommended sample size:", round(ss_high, 3), "\n")
    } else {
      stop("\nArgument 'prop' must be a number between 0 and 1\n")
    }
  } else {
    # Calcular tamaños de muestra para regresión lineal
    ss_optimal <- (df * 15)
    ss_low <- (df * 10)
    ss_high <- (df * 20)

    # Almacenar texto en la lista
    text_outp$main <- paste("\nSample Size Calculation for a multiple linear regression with", df, "degrees of freedom\n", "\n")
    text_outp$text_min <- paste("Minimum recommended sample size:", round(ss_low, 3), "\n")
    text_outp$text_opt <- paste("Recommended sample size:", round(ss_optimal, 3), "\n")
    text_outp$text_max <- paste("Maximum recommended sample size:", round(ss_high, 3), "\n")
  }

  ss_multreg_obj<-list(
    ss_optimal = NULL,
    ss_low = NULL,
    ss_high = NULL
  )

  ss_multreg_obj$ss_optimal <- ss_optimal
  ss_multreg_obj$ss_low <- ss_low
  ss_multreg_obj$ss_high <- ss_high

  if(verbose){
    cat(paste(unlist(text_outp), collapse = ""))
  }

  class(ss_multreg_obj) <- "ss_multreg_obj"

  invisible(ss_multreg_obj)
}

