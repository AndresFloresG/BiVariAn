#' @importFrom purrr map_chr
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr where
#' @importFrom readr type_convert
#' @name fix_mixed_cols
#' @title Fix mixed cols in dataframes
#'
#' @param df Dataframe to fix
#'
#' @returns A dataframe with columns with columns without list
#'
#' @examples
#'
#' col_1 <- c(1, 3, 4, 5, 5, 2)
#' col_2 <- c(1, 2, 3, 4, 5, 6)
#' col_3 <- c("a", "b", "c", "d", "e", "f")
#'
#'
#' df <- data.frame(col_1, col_2, col_3)
#'
#' df$col_1 <- as.list(df$col_1) # Reproduce an importing error
#'
#' str(df)
#'
#' fixed_df <- fix_mixed_cols(df)
#' str(fixed_df)
#'
#'
#' @export
fix_mixed_cols <- function(df){
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.list),
        ~ purrr::map_chr(.x, function(v) {
          # NA o vacío -> NA_character_
          if (length(v) == 0 || all(is.na(v))) return(NA_character_)
          # Si trae múltiples elementos, júntalos con coma
          paste(as.character(v), collapse = ",")
        })
      )
    ) %>%
    # Vuelve a inferir tipos: si una columna quedó puramente numérica, pasa a numeric/integer
    readr::type_convert()
}
