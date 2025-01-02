#' @importFrom magrittr '%>%'
#' @title Encode character variables as factor automatically
#' @name encode_factors
#' @aliases encode_factors
#' @param data Dataframe to be encoded
#' @param encode Column class to be encoded. Must be "character" or "integer"
#' @param list_factors List of factors to be encoded
#' @param uselist Logical operator to determine if use list of factors or not. If TRUE, list_factors argument must be provided.
#'
#' @returns Converts listed columns to factors.
#'
#' @examples
#' df <- data.frame(has = c("Yes", "No", "Yes", "Yes", "No", "No", "Yes"),
#' smoke = c("Yes", "No", "No", "Yes", "No", "Yes", "No"),
#' gender = c("Male", "Female", "Male", "Female", "Female", "Male", "Male"))
#'
#' str(df)
#'
#' df <- encode_factors(df, encode = "character")
#'
#' str(df)
#'
#'
#' @export
#'
encode_factors <- function(data, encode = c("character", "integer"), list_factors = NULL, uselist = FALSE) {
  # Validate that data is a data frame

  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame")
  }

  # Validate that the user provided a non-empty list if uselist is TRUE
  if (uselist && is.null(list_factors)) {
    stop("If 'uselist' is TRUE, 'list_factors' cannot be NULL")
  }

  if(is.character(uselist) | !is.logical(uselist)){
    stop("uselist must be a logical argument")
  }

  if (uselist) {
    # Check if the columns in list_factors exist in data
    if (!all(list_factors %in% names(data))) {
      stop("At least one column in 'list_factors' does not exist in the data")
    }
    # Convert the specified columns to factors
    data[list_factors] <- lapply(data[list_factors], as.factor)
  } else if (encode[1] == "character") {
    # Convert columns of type character to factors
    data <- data %>% mutate(across(where(is.character), as.factor))
  } else if (encode[1] == "integer") {
    # Convert columns of type integer to factors
    data <- data %>% mutate(across(where(is.integer), as.factor))
  } else {
    stop("The 'encode' argument must be either 'character' or 'integer'")
  }

  # Return the modified data frame
  return(data)
}




