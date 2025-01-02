#' @import ggplot2
#' @import ggprism
#' @name auto_viol_cont
#' @author JMCR
#' @title auto_viol_cont
#' @aliases auto_viol_cont
#' @description
#' Automatically generates violinplots of continuous variables from a database and a grouping variable. The names of the variables are set to the names defined in the database. As a result, graphs generated with the default theme "theme_serene" will be obtained. In this function it is not possible to use labels for the variables, use "auto_viol_cont_wlabels" instead.
#'
#' @param data Name of the dataframe
#' @param groupvar Name of the grouping variable
#' @param violinplot_args List of arguments to be passed to "geom_violin"
#' @param theme_func Theme to display plots. Default is "theme_serene"
#' @param lang_labs Language of the resulting plots. Can be "EN" for english or "SPA" for spanish. Default is "SPA".
#'
#'
#' @export


auto_viol_cont <- function(data, groupvar,
                         violinplot_args = list(),
                         theme_func = theme_serene,
                         lang_labs = c("EN", "SPA")) {
  # Verificar si theme_func es una funcion
  if (!is.function(theme_func)) {
    stop("Argument 'theme_func' must be a valid function")
  }

  if(any(is.null(lang_labs) | lang_labs == "SPA")){
    titlab1 = "Box plot de"
    titlab2 = "por"
  } else if (lang_labs == "EN"){
    titlab1 = "Bar plot of"
    titlab2 = "by"
  }

  # Identificar variables continuas
  variables_continuas <- colnames(data %>% select_if(is.numeric))

  # Crear una lista para almacenar las graficas
  graficascont <- list()

  # Bucle para generar y almacenar graficas
  for (var2 in variables_continuas) {
    if (var2 %in% names(data)) {  # Verifica si la variable esta en la base de datos
      # Crear la base de la grafica

      lab_viol_var<- if(!is.null(label(data[[var2]]))) label(data[[var2]]) else var2
      lab_viol_group<- if(!is.null(label(data[[groupvar]]))) label(data[[groupvar]]) else groupvar

      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[groupvar]], y = .data[[var2]]))

      # Agregar geom_violinplot con argumentos personalizados
      p <- p + do.call(ggplot2::geom_violin, violinplot_args)

      # Agregar etiquetas y tema
      p <- p + ggplot2::labs(
        title = paste(titlab1, lab_viol_var, titlab2, lab_viol_group),
        x = groupvar,
        y = var2
      ) +
        theme_func() +
        ggplot2::theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
        )

      # Guardar la grafica en la lista
      graficascont[[var2]] <- p
    } else {
      cat("\nVariable", var2, "is not in provided dataframe\n")
    }
  }

  # Retornar la lista de graficas
  return(graficascont)
}
