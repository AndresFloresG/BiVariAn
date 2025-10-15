#' @importFrom purrr map_chr
#' @importFrom readr type_convert
#' Fix mixed cols in dataframes
#'
#' @param df Dataframe to fix
#'
#' @returns A dataframe with columns with columns without list
#'
#' @examples
#' df <- data.frame
#'
#' fixed_df <- fix_mixed_cols(df)
#'
#' @export
fix_mixed_cols <- function(df) {
  df %>%
    mutate(
      across(
        where(is.list),
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
