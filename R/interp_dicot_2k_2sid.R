#' @importFrom epitools oddsratio
#' @importFrom stats chisq.test
#' @importFrom stats fisher.test
#'
#' @name interp_dicot_2k_2sid
#' @title Generic interpretation of chi squared or Fisher test
#' @usage interp_dicot_2k_2sid(data, var1, var2)
#' @param data NAme of the database
#' @param var1 a factor object, must be 2 levels
#' @param var2 a factor object, must be 2 levels
#' @description
#' It automatically writes a generic interpretation for the Chi-square test or Fisher's exact test as appropriate. In addition, it prints the contingency table for the variables included, as well as the raw statistical tests. Currently it only returns text in Spanish, more languages are planned to be added in the future.
#'
#' @return It returns as output a generic text according to certain conditions which are as follows:
#' Format 1: Chi-square with statistical significance. Conditions: Minimum expected frequencies >5, p<0.05.
#' Format 2: Chi-square without statistical significance. Conditions: Minimum expected frequencies >5, p>0.05.
#' Format 3: Fishers exact test with statistical significance. Conditions: Minimum expected frequencies <5, p<0.05. Format 4: Exact fisher's test without statistical significance. Conditions: Minimum expected frequencies <5, p>0.05.
#'
#' @export

interp_dicot_2k_2sid <- function(data, var1, var2) {
  # Convertir las variables en factores
  data[[var1]] <- as.factor(data[[var1]])
  data[[var2]] <- as.factor(data[[var2]])

  # Crear tabla de contingencia
  tabla <- table(data[[var1]], data[[var2]])

  # Verificar si la tabla tiene al menos dos categorias validas
  if (all(dim(tabla) > 1)) {
    # Prueba de Chi cuadrada
    chi_test <- tryCatch(
      chisq.test(tabla, simulate.p.value = FALSE),
      error = function(e) NULL
    )

    # Prueba exacta de Fisher
    fisher_test <- tryCatch(
      fisher.test(tabla),
      error = function(e) NULL
    )

    # Calcular Odds Ratio e IC 95%
    or <- tryCatch(
      epitools::oddsratio(tabla, method = "wald"),
      error = function(e) list(measure = matrix(NA, nrow = 1, ncol = 3))
    )
    odds_ratio <- if (!is.null(or$measure)) or$measure[2, "estimate"] else NA
    ci_lower <- if (!is.null(or$measure)) or$measure[2, "lower"] else NA
    ci_upper <- if (!is.null(or$measure)) or$measure[2, "upper"] else NA

    # Condiciones y seleccion del formato de texto
    if (!is.null(chi_test)) {
      min_expected <- min(chi_test$expected)
      chi_p_value <- chi_test$p.value

      if (min_expected > 5) { # Chi cuadrada valida
        if (chi_p_value < 0.05) {
          # Formato 1: Chi cuadrada con significancia estadistica
          texto <- paste(
            "La prueba de Chi cuadrada mostro una asociacion estadisticamente significativa entre",
            var1, "y", var2, ". El estadistico de Chi cuadrada fue",
            round(chi_test$statistic, 2), "con un valor p de",
            format.pval(chi_p_value, digits = 3), ". Todas las frecuencias esperadas fueron mayores a 5.",
            "Se obtuvo un Odds Ratio de", round(odds_ratio, 2), "con IC95 [",
            round(ci_lower, 2), ",", round(ci_upper, 2), "]."
          )
        } else {
          # Formato 2: Chi cuadrada sin significancia estadistica
          texto <- paste(
            "La prueba de Chi cuadrada no mostro una asociacion estadisticamente significativa entre",
            var1, "y", var2, ". El estadistico de Chi cuadrada fue",
            round(chi_test$statistic, 2), "con un valor p de",
            format.pval(chi_p_value, digits = 3), ". Todas las frecuencias esperadas fueron mayores a 5.",
            "Se obtuvo un Odds Ratio de", round(odds_ratio, 2), "con IC95 [",
            round(ci_lower, 2), ",", round(ci_upper, 2), "]."
          )
        }
      } else if (!is.null(fisher_test)) { # Usar Fisher si las frecuencias esperadas son <5
        fisher_p_value <- fisher_test$p.value
        if (fisher_p_value < 0.05) {
          # Formato 3: Test exacto de Fisher con significancia estadistica
          texto <- paste(
            "Dado que algunas frecuencias esperadas fueron menores a 5, se realizo el test exacto de Fisher.",
            "El analisis mostro una asociacion estadisticamente significativa entre",
            var1, "y", var2, "con un valor p de",
            format.pval(fisher_p_value, digits = 3), ".",
            "Se obtuvo un Odds Ratio de", round(odds_ratio, 2), "con IC95 [",
            round(ci_lower, 2), ",", round(ci_upper, 2), "]."
          )
        } else {
          # Formato 4: Test exacto de Fisher sin significancia estadistica
          texto <- paste(
            "Dado que algunas frecuencias esperadas fueron menores a 5, se realizo el test exacto de Fisher.",
            "El analisis no mostro una asociacion estadisticamente significativa entre",
            var1, "y", var2, "con un valor p de",
            format.pval(fisher_p_value, digits = 3), ".",
            "Se obtuvo un Odds Ratio de", round(odds_ratio, 2), "con IC95 [",
            round(ci_lower, 2), ",", round(ci_upper, 2), "]."
          )
        }
      } else {
        texto <- "No se pudo realizar un analisis estadistico debido a restricciones en los datos."
      }
    } else {
      texto <- "No se pudo realizar un analisis estadistico debido a restricciones en los datos."
    }

    # Imprimir el texto y la tabla de contingencia
    cat(texto, "\n\n")
    print(tabla)
    print(chisq.test(tabla, simulate.p.value = FALSE))
    print(fisher.test(tabla))
    print(paste("Minimum expected frequencies", min_expected))
    print(paste("OR ", odds_ratio, "IC95 [", ci_lower, ", ", ci_upper))

  } else {
    # Si la tabla no es valida, devolver mensaje de error
    cat("La tabla de contingencia no tiene al menos dos categorias validas en cada dimension.\n")
  }
}
