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
#'
#' @export


auto_viol_cont <- function(data, groupvar,
                         violinplot_args = list(),
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

      # Agregar geom_violinplot con argumentos personalizados
      p <- p + do.call(ggplot2::geom_violin, violinplot_args)

      # Agregar etiquetas y tema
      p <- p + ggplot2::labs(
        title = paste("Distribucion de", var2),
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
      cat("\nLa variable", var2, "no esta presente en la base de datos.\n")
    }
  }

  # Retornar la lista de graficas
  return(graficascont)
}
