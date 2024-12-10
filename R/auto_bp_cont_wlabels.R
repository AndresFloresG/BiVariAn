#' @import ggplot2
#' @import ggprism
#' @import table1
#' @name auto_bp_cont_wlabels
#' @author JMCR
#' @title auto_bp_cont_wlabels (Deprecated)
#' @aliases auto_bp_cont_wlabels
#' @description
#' Deprecated, use "auto_bp_cont" instead.
#' #' Automatically generates boxplot plots of continuous variables from a database and a grouping variable. The names of the variables are set to the names defined in the database. As a result, graphs generated with the default theme "theme_serene" will be obtained. In this function, the user must define each variable label with "label" function from "table1" package.
#' @param data Name of the dataframe
#' @param groupvar Name of the grouping variable
#' @param boxplot_args List of arguments to be passed to "geom_violin"
#' @param theme_func Theme to display plots. Default is "theme_serene"
#'
#' @export

auto_bp_cont_wlabels <- function(data, groupvar,
                         boxplot_args = list(),
                         theme_func = theme_serene) {
  # Verificar si theme_func es una funcion
  if (!is.function(theme_func)) {
    stop("El argumento 'theme_func' debe ser una funcion de tema valida.")
  }

  # Identificar variables continuas
  variables_continuas <- colnames(data %>% select_if(is.numeric))

  # Crear una lista para almacenar las graficas
  graficascont <- list()

  # Bucle para generar y almacenar graficas
  for (var2 in variables_continuas) {
    if (var2 %in% names(data)) {  # Verifica si la variable esta en la base de datos
      # Crear la base de la grafica
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = groupvar, y = var2))

      # Agregar geom_boxplot con argumentos personalizados
      p <- p + do.call(ggplot2::geom_boxplot, boxplot_args)

      # Agregar etiquetas y tema
      p <- p + ggplot2::labs(
        title = paste("Distribucion de", label(data[[var2]])),
        x = label(data[[groupvar]]),
        y = label(data[[var2]])
      ) +
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
