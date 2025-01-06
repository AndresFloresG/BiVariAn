#' @import ggplot2
#' @importFrom table1 label
#' @title Generates automatic scatterplot with correlation plot
#' @name auto_corr_cont
#'
#' @author JMCR
#' @aliases auto_corr_cont
#' @description
#' Automatically generates correlation plots of continuous variables from a database and a reference variable. The names of the variables are set to the names defined in the database. As a result, graphs generated with the default theme "theme_serene" will be obtained. In this function, the user must define each variable label with "label" function from "table1" package
#'
#' @param data Dataframe from which variables will be extracted
#' @param referencevar Reference variable. Must be continuous variable as string (quoted)
#' @param point_args List containing extra arguments to be passed to geom_point function. If no specified, only "stat="identity"" will be passed
#' @param smooth_args List containing extra arguments to be passed to geom_smooth function. If no specified, only "method="lm"" will be passed
#' @param theme_func Theme to display plots. Default is "theme_serene"
#' @param lang_labs Language to display title lab. Default is Spanish.
#'
#' @return Returns a list containing barplots as ggplot2 objects. Objects can be accessed via $ operator.
#'
#' @examples
#' data <- data.frame(group = rep(letters[1:2], 30),
#' var1 = rnorm(30, mean = 15, sd = 5),
#' var2 = rnorm(30, mean = 20, sd = 2),
#' var3 = rnorm(30, mean = 10, sd = 1),
#' var4 = rnorm(30, mean = 5, sd =2))
#'
#' cont_corrplot <- auto_corr_cont(data = data, referencevar = "var1", lang_labs = "EN")
#'
#' # Call to show all storaged plots
#' cont_corrplot
#'
#' # Call to show one individual plot
#' cont_corrplot$var2
#'
#' @export

auto_corr_cont<- function(data,
                          referencevar = NULL,
                          point_args=list(),
                          smooth_args=list(),
                          theme_func = theme_serene,
                          lang_labs = c("EN", "SPA")){


  if (!is.function(theme_func)) {
    stop("Argument 'theme_func' must be a valid function")
  }

  if(is.null(referencevar)){
    stop("referencevar cannot be NULL")
  }

  if(!(referencevar %in% names(data))){
    stop("referencevar is not in database")
  }

  if (!is.numeric(data[[referencevar]])) {
    stop("referencevar variable must be numerical.")
  }

  default_point_args <- list(stat = "identity")
  default_smooth_args <- list(method = "lm")


  if (length(point_args) == 0){
    point_args = default_point_args
  } else {
    point_args = modifyList(default_point_args, point_args)
  }

  if (length(smooth_args) == 0){
    smooth_args = default_smooth_args
  } else {
    smooth_args = modifyList(default_smooth_args, smooth_args)
  }

  if (any(is.null(lang_labs) | lang_labs == "SPA" )){
    titleleg1 <- paste("Correlaci\u00f3n entre")
    titleleg2 <- paste("con")

  } else if (lang_labs == "EN"){
    titleleg1 <- paste("Correlation between")
    titleleg2 <- paste("and")
  }



  cont_var <- colnames(data) [sapply(data, function(x) is.numeric(x) && !identical(x, data[[referencevar]]))]


  contgraf <- list()

  for(variable in cont_var){
    if(variable %in% names(data)){

      lab_corr_var_var<-if(!is.null(table1::label(data[[variable]]))) table1::label(data[[variable]]) else variable

      lab_corr_var_ref<-if(!is.null(table1::label(data[[referencevar]]))) table1::label(data[[referencevar]]) else referencevar

      p <- ggplot2::ggplot(data, ggplot2::aes(x=.data[[variable]], y=.data[[referencevar]]))
      p <- p + do.call(ggplot2::geom_point, point_args)
      p <- p + do.call(ggplot2::geom_smooth, smooth_args)
      p <- p + ggplot2::labs(
        title = paste(titleleg1, referencevar, titleleg2, variable),
        x = lab_corr_var_var,
        y = lab_corr_var_ref)+
        theme_func()+
        ggplot2::theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


      contgraf[[variable]] <- p

    } else {
      if (lang_labs == "EN"){
        cat("\nVariable", variable, "is not present in database \n")
      } else if(lang_labs == "SPA"){
        cat("\nLa variable", variable, "no esta presente en la base de datos.\n")
      }
    }
  }
  return(contgraf)
}
