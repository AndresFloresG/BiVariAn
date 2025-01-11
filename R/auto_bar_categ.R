#' @import ggplot2
#' @import ggprism
#' @importFrom table1 label
#' @importFrom scales percent
#' @importFrom magrittr '%>%'
#' @name auto_bar_categ
#' @title Automatic generation of barplot with percentages
#' @description
#' Automatically generates barplot stratified by group variables with or without percentages.
#'
#' @param data Name of the dataframe
#' @param groupvar Name of the grouping variable. Grouping variable will be used in "fill" for aesthetics argument in the creation of each ggplot object. If not provided, the function take each variable as grouping and does not display the "fill" legend.
#' @param bar_args List of arguments to be passed to "geom_bar". If `NULL`, the function uses default arguments such as:
#' * position = "dodge"
#' * colour = "black"
#' * linewidth = 0.9
#' * alpha = 0.5
#' @param theme_func Theme of the generated plots. Must be the name of the function without parenthesis. Use for example: `theme_minimal` instead of `theme_minimal()`
#' @param lang_labs Language of displayed labels. If null, default is spanish.
#' @param showpercent Logical atribute to indicate if the graph should include percentages
#'
#' @return Returns a list containing all barplots as ggplot object. Can be accessed via $ operator
#'
#' @examples
#' data<-data.frame(categ = rep(letters[1:2], 10),
#' var1 = rep(LETTERS[4:5], 10),
#' var2 = rep(LETTERS[6:7], 10),
#' var3 = rep(LETTERS[8:9], 10),
#' var4 = rep(LETTERS[10:11], 10))
#'
#' data$categ <- as.factor(data$categ)
#' data$var1 <- as.factor(data$var1)
#' data$var2 <- as.factor(data$var2)
#' data$var3 <- as.factor(data$var3)
#' data$var4 <- as.factor(data$var4)
#'
#' barplot_list<-auto_bar_categ(data = data, groupvar = "categ", lang_labs = "EN")
#'
#' barplot_list$var1
#'
#'
#' # Example using `groupvar` argument as `NULL`
#' auto_bar_categ(data = data)$var2
#'
#'
#' @export
#'
#'

auto_bar_categ <- function(data,
                           groupvar = NULL,
                           bar_args = list(),
                           theme_func = theme_serene,
                           lang_labs = c("EN", "SPA"),
                           showpercent = TRUE) {
  # Verificar que el argumento de tema sea una funcion valida
  if (!is.function(theme_func)) {
    stop("El argumento 'theme_func' debe ser una funcion de tema valida.")
  }

  if(!is.null(groupvar)){
    if (!groupvar %in% names(data)) {
      stop("The grouping variable must be a column in the data frame.")
    }
    if (!is.factor(data[[groupvar]]) && !is.character(data[[groupvar]])) {
      stop("The grouping variable must be categorical.")
    }

    if(is.character(showpercent) || !is.logical(showpercent)){
      stop("showpercent must be a logical argument")
    }

  }

  if (any(is.null(lang_labs) | lang_labs == "SPA")){
    titlelab = "Distribuci\u00f3n de"
    ylabs= "Frecuencia"
  } else if(lang_labs == "EN"){
    titlelab = "Distribution of"
    ylabs= "Frequency"
  }

  if(!is.data.frame(data)){
    stop("data must be a data.frame object")
  }

  bar_args_default <- list(position = "dodge",
                           colour = "black",
                           linewidth = 0.9,
                           alpha = 0.5)


  if(length(bar_args) == 0) {
    bar_args = bar_args_default
  } else {
    bar_args = modifyList(bar_args_default, bar_args)
  }

  if(!is.null(groupvar)){
  categ_var <- colnames(data)[sapply(data, function(x) is.factor(x) && !identical(x, data[[groupvar]]))]
  } else {
    categ_var <- colnames(data)[sapply(data, function(x) is.factor(x))]
  }

  graficas <- list()

  for (varcat in categ_var) {
    if (varcat %in% names(data)) {
      lab_graf_cat <- if (!is.null(table1::label(data[[varcat]]))) table1::label(data[[varcat]]) else varcat

      if(!is.null(groupvar)){
        lab_graf_group <- if (!is.null(table1::label(data[[groupvar]]))) table1::label(data[[groupvar]]) else groupvar
      } else {
        lab_graf_group <- if (!is.null(table1::label(data[[varcat]]))) table1::label(data[[varcat]]) else varcat
      }

      if(is.null(groupvar)){
        group_fill = varcat
      } else group_fill = groupvar

      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[varcat]], fill = .data[[group_fill]])) +
        do.call(geom_bar, bar_args)

      if (showpercent){
        p <- p + ggplot2::geom_text(stat = "count",
                  aes(label=scales::percent(after_stat(count/sum(count)))),
                  position=position_dodge(width=0.9),
                  vjust=-.5)
      }

        p <- p + ggplot2::labs(
          title = paste(titlelab, lab_graf_cat),
          x = lab_graf_cat,
          y = ylabs,
          fill = lab_graf_group
        )

        if(!is.null(groupvar)){
          p <- p + ggplot2::scale_y_continuous(limits = c(0, (max(table(data[[varcat]], data[[groupvar]])))+10))+
            theme_func() +
            theme(
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
        } else if (is.null(groupvar)){
          p <- p + ggplot2::scale_y_continuous(limits = c(0, max(table(data[[varcat]])) +10))+
            theme_func() +
            theme(
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
        }


      if(is.null(groupvar)){
        p <- p + theme(legend.position = "none")
      }


      graficas[[varcat]] <- p
    } else {
      if (any(is.null(lang_labs) | lang_labs == "SPA")){
        warning("\nLa variable", varcat, "no esta presente en la base de datos.\n")
      } else if (lang_labs == "EN"){
        warning("\nVariable", varcat, "is not in the provided dataframe \n")
      }
    }
  }

  return(graficas)
}
