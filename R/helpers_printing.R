# Internal helpers for p-value formatting
# These functions are not exported (@noRd)

#' Format p-values for presentation
#'
#' Converts numeric p-values to presentation-ready strings with controlled
#' precision. Values below threshold are shown as "<threshold".
#'
#' @param p Numeric p-value(s) to format. Can be vector, NA, NaN, or character
#'   that can be coerced to numeric.
#' @param digits Integer number of decimal places (default: 3).
#' @param threshold Numeric threshold below which p-values are shown as
#'   "<threshold" (default: 0.001).
#'
#' @return Character vector of formatted p-values. Returns "NA" for missing/
#'   invalid values, and the input as string for values outside [0,1] range.
#'
#' @noRd
#'
#' @examples
#' .format_pvalue(0.049) # "0.049"
#' .format_pvalue(0.0496) # "0.050"
#' .format_pvalue(8.12e-17) # "<0.001"
#' .format_pvalue(c(0.03, NA, 0.5)) # c("0.030", "NA", "0.500")
.format_pvalue <- function(p, digits = 3, threshold = 0.001) {
  # Handle character input - try to convert to numeric
  if (is.character(p)) {
    p <- suppressWarnings(as.numeric(p))
  }

  # Initialize output vector
  result <- character(length(p))

  # Handle each element
  for (i in seq_along(p)) {
    p_i <- p[i]

    # Case 1: NA or NaN
    if (is.na(p_i)) {
      result[i] <- "NA"
      next
    }

    # Case 2: Values outside [0, 1] range - return as-is but as string
    if (p_i < 0 || p_i > 1) {
      result[i] <- as.character(p_i)
      next
    }

    # Case 3: Exactly zero - format normally (special case)
    if (p_i == 0) {
      formatted <- format(0, nsmall = digits, scientific = FALSE)
      formatted <- trimws(formatted)
      result[i] <- formatted
      next
    }

    # Case 4: Below threshold (but not zero)
    if (p_i < threshold) {
      # Format threshold with same number of digits
      threshold_str <- format(threshold, nsmall = digits, scientific = FALSE)
      # Remove trailing zeros if any
      threshold_str <- sub("0+$", "", threshold_str)
      threshold_str <- sub("\\.$", "", threshold_str)
      result[i] <- paste0("<", threshold_str)
      next
    }

    # Case 5: Normal formatting with specified digits
    # Use format() with nsmall to ensure trailing zeros
    formatted <- format(round(p_i, digits), nsmall = digits, scientific = FALSE)
    # Remove leading spaces that format() might add
    formatted <- trimws(formatted)
    result[i] <- formatted
  }

  return(result)
}


#' Add significance symbols to formatted p-values
#'
#' Appends standard significance symbols to formatted p-value strings based on
#' the raw numeric p-value and specified thresholds.
#'
#' @param p_raw Numeric vector of raw p-values used to determine significance.
#'   Can contain NA, NaN, or values outside [0,1].
#' @param thresholds Numeric vector of significance thresholds in descending
#'   order (default: c(0.001, 0.01, 0.05)).
#' @param symbols Character vector of symbols corresponding to thresholds
#'   (default: c("***", "**", "*")).
#'
#' @return Character vector with significance symbols appended. No symbol is
#'   added for non-significant values or invalid inputs (NA, NaN, out of range).
#'
#' @noRd
#'
#' @examples
#' p_raw <- c(8.12e-17, 0.03, 0.21, NA)
#' .add_significance(p_raw)
.add_significance <- function(p_raw,
                              thresholds = c(0.001, 0.01, 0.05),
                              symbols = c("***", "**", "*"),
                              format_p = TRUE,
                              digits = 3) {
  if (length(thresholds) != length(symbols)) {
    stop("'thresholds' y 'symbols' deben tener la misma longitud.")
  }

  # Ordenar thresholds de menor a mayor (y sus símbolos asociados)
  ord <- order(thresholds)
  thresholds <- thresholds[ord]
  symbols <- symbols[ord]


  # Inicializar como character para evitar coerciones silenciosas
  result <- character(length(p_raw))

  for (i in seq_along(p_raw)) {
    p_i <- p_raw[i]

    # ── Casos inválidos ────────────────────────────────────────────────────
    if (is.na(p_i)) {
      result[i] <- NA_character_
      next
    }

    if (!is.numeric(p_i)) {
      result[i] <- as.character(p_i)
      next
    }

    if (p_i < 0 || p_i > 1) {
      result[i] <- as.character(p_i)
      next
    }

    # ── Formatear el p-value usando el numérico original (p_i, no result[i]) ──
    p_fmt <- if (format_p) {
      .format_pvalue(p_i, digits = digits)
    } else {
      as.character(p_i)
    }


    # Nivel de símbolo: <= en umbrales internos (p = 0.01 sí recibe **)
    # Thresholds están en orden creciente; el primer match es el más estricto aplicable
    symbol_to_add <- ""
    for (j in seq_along(thresholds)) {
      if (p_i <= thresholds[j]) {
        symbol_to_add <- symbols[j]
        break
      }
    }

    result[i] <- paste0(p_fmt, symbol_to_add)
  }

  return(result)
}
