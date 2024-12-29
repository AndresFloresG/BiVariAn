#' @importFrom pak pak
#' @importFrom utils packageVersion
#' @title Update BiVariAn itself
#' @name BiVariAn_update
#' @aliases BiVariAn_update
#' @description
#' Use this function to update the development version of BiVariAn
#' @param force Whether to force an update, even if no newer version is available. (Currently even if FALSE, the package will update... working on process)
#'
#' @export
#'
BiVariAn_update <- function(force = FALSE) {

  stopifnot(is.logical(force), length(force) == 1)

  repo <- "AndresFloresG/BiVariAn"
  installed <- utils::packageVersion("BiVariAn")


  if (is.na(installed)) {
    message("\nBiVariAn is not installed. Installing the latest version from GitHub...\n")
    pak::pak(repo)
    return(invisible())
  }

  pak::pak(repo)

  message("\nUpdating BiVariAn")
  pak::pak(repo)

  message("\nReloading BiVariAn...\n")
  tryCatch({
    detach("package:BiVariAn", unload = TRUE)
    library(BiVariAn, character.only = TRUE)
    message("\nBiVariAn successfully updated to version ", packageVersion("BiVariAn"), ".\n")
  }, error = function(e) {
    warning("\nFailed to reload BiVariAn. Please restart your R session to use the updated version.\n")
  })

  invisible()
}
