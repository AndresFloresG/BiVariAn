#' @import dplyr
#' @importFrom rrtable df2flextable
#'
#' @importFrom tableHTML tableHTML
#' @importFrom tableHTML add_theme
#' @author JAFG
#' @name auto_shapiro_raw
#' @aliases auto_shapiro_raw
#' @title Automatic Shapiro-Wilk test table
#' @description
#' Generates a HTML table of raw data from a numerical variables of a dataframe.
#' @param data Data frame from which variables will be extracted.
#' @param flextableformat Logical operator to indicate the output desired. Default is TRUE. When FALSE, function will return a dataframe format.
#'
#' @examples
#' auto_shapiro_raw(iris)

#' @export


auto_shapiro_raw <- function(data, flextableformat= TRUE){
  var_cont<-c(colnames(data %>% select_if(is.numeric)))

  resultados<-list()

  for (var1 in var_cont) {
    if (var1 %in% names(data)) {
      shapiro_p <- stats::shapiro.test((data[[var1]]))$p.value

      resultados[[var1]]<- list(
        Variable= var1,
        p_shapiro=  ifelse(shapiro_p>0.001, paste(round(shapiro_p, digits=5)), "<0.001*" ),
        Normality=  paste(ifelse(shapiro_p>0.05, "Normal", "Non-normal" ))
      )
    } else {
      cat("\nLa variable", var1, "no esta presente en la base de datos.\n")
    }
  }

  resultadosdf <-do.call(rbind, lapply(resultados, as.data.frame))

  if (flextableformat == TRUE){
    return(rrtable::df2flextable(resultadosdf, vanilla = TRUE))
  }
  else {
    return(resultadosdf)
  }

}
