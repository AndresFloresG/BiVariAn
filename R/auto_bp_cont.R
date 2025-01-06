#' @import ggplot2
#' @import ggprism
#' @name auto_bp_cont
#' @author JMCR
#' @title auto_bp_cont
#' @aliases auto_bp_cont
#' @description
#' Automatically generates boxplot plots of continuous variables from a database and a grouping variable. The names of the variables are set to the names defined in the database. As a result, graphs generated with the default theme "theme_serene" will be obtained. In this function, the user must define each variable label with "label" function from "table1" package.
#'
#' @param data Name of the dataframe
#' @param groupvar Name of the grouping variable
#' @param boxplot_args List of arguments to be passed to "geom_bar"
#' @param theme_func Theme to display plots. Default is "theme_serene"
#' @param lang_labs Language of the resulting plots. Can be "EN" for english or "SPA" for spanish. Default is "SPA"
#'
#' @returns A list containing ggplot2 objects with generated plots. Each element can be accessed by using $ operator.
#'
#' @examples
#' data <- data.frame(group = rep(letters[1:2], 30),
#' var1 = rnorm(30, mean = 15, sd = 5),
#' var2 = rnorm(30, mean = 20, sd = 2),
#' var3 = rnorm(30, mean = 10, sd = 1),
#' var4 = rnorm(30, mean = 5, sd =2))
#'
#' data$group<-as.factor(data$group)
#'
#' # Create a list containing all the plots
#' boxplots<-auto_bp_cont(data = data, groupvar = 'group', lang_labs = 'EN')
#'
#' # call to show all storaged plots
#' boxplots
#'
#' # call to show one individual plots
#' boxplots$var1
#'
#' @export

auto_bp_cont <- function(data,
                         groupvar,
                         boxplot_args = list(),
                         theme_func = theme_serene,
                         lang_labs = c("EN", "SPA")) {
  # Verificar si theme_func es una funcion
  if (!is.function(theme_func)) {
    stop("El argumento 'theme_func' debe ser una funcion de tema valida.")
  }

  if(!is.data.frame(data)){
    stop("data must be a data.frame object")
  }

  if(any(is.null(lang_labs) | lang_labs == "SPA")){
    titlab1 = "Box plot de"
    titlab2 = "por"
  } else if (lang_labs == "EN"){
    titlab1 = "Bar plot of"
    titlab2 = "by"
  }

  if (!groupvar %in% names(data)) {
    stop("The grouping variable must be a column in the data frame.")
  }

  if (!is.factor(data[[groupvar]]) && !is.character(data[[groupvar]])) {
    stop("The grouping variable must be categorical.")
  }

  default_bp_args <- list(
    fill = "white",
    alpha = 0
  )

  if(length(boxplot_args) == 0){
    boxplot_args = default_bp_args
  } else {
    boxplot_args = modifyList(default_bp_args, boxplot_args)
  }

  # Identificar variables continuas
  variables_continuas <- colnames(data %>% select_if(is.numeric))

  # Crear una lista para almacenar las graficas
  graficascont <- list()

  # Bucle para generar y almacenar graficas
  for (var2 in variables_continuas) {
    if (var2 %in% names(data)) {  # Verifica si la variable esta en la base de datos
      lab_bp_var<- if(!is.null(label(data[[var2]]))) label(data[[var2]]) else var2
      lab_bp_group<- if(!is.null(label(data[[groupvar]]))) label(data[[groupvar]]) else groupvar
      # Crear la base de la grafica
      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[groupvar]], y = .data[[var2]]))

      # Agregar geom_boxplot con argumentos personalizados
      p <- p + do.call(ggplot2::geom_boxplot, boxplot_args)

      # Agregar etiquetas y tema
      p <- p + ggplot2::labs(
        title = paste(titlab1, lab_bp_var, titlab2, lab_bp_group),
        x = lab_bp_group,
        y = lab_bp_var) +
        theme_func() +
        ggplot2::theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
        )

      # Guardar la grafica en la lista
      graficascont[[var2]] <- p
    } else {
      cat("\nLa variable", var2, "no esta presente en la base de datos.\n")
    }
  }

  # Retornar la lista de graficas
  return(graficascont)
}


