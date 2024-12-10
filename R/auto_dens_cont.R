#' @import ggplot2
#' @import ggprism
#' @import table1
#' @name auto_dens_cont
#' @author JMCR
#' @title auto_dens_cont
#' @aliases auto_dens_cont
#' @description
#' #' Automatically generates density plots of continuous variables from a database. The names of the variables are set to the names defined in the database. As a result, graphs generated with the default theme "theme_serene" will be obtained. In this function, the user must define each variable label with "label" function from "table1" package.
#' @param data Name of the dataframe
#' @param densplot_args List of arguments to be passed to "geom_density"
#' @param theme_func Theme to display plots. Default is "theme_serene"
#' @param s_mean Show mean. Logical operator to indicate if the mean should be plotted. Default is TRUE
#' @param s_median Show median. Logical operator to indicate if the median should be plotted. Default is TRUE
#' @param c_mean Color of the line indicating the mean. Default is red.
#' @param c_median Color of the line indicating the median. Default is blue.
#' @param lt_mean Linetype of the line indicating the mean. Default is "solid".
#' @param lt_median Linetype of the line indicating the median. Default is "dotdash".
#' @param lw_mean Linewidth of the line indicating the mean. Default is 1.
#' @param lw_median Linewidth of the line indicating the mean. Default is 1.
#'
#' @returns Returns a list containing the generated density plots
#'
#' @export
#'

auto_dens_cont <- function(data,
                           s_mean = TRUE,
                           s_median = TRUE,
                           c_mean = "red",
                           c_median = "blue",
                           lt_mean = "solid",
                           lt_median = "dotdash",
                           lw_mean = 1,
                           lw_median = 1,
                           densplot_args = list(),
                           theme_func = theme_serene) {
  # Verificar que el argumento de tema sea una funcion
  if (!is.function(theme_func)) {
    stop("El argumento 'theme_func' debe ser una funcion de tema valida.")
  }

  # Seleccionar variables continuas (numericas)
  contvariables <- colnames(data %>% select_if(is.numeric))

  # Lista para almacenar las graficas
  grafdenscon <- list()

  # Bucle para generar las graficas
  for (contvar in contvariables) {
    if (contvar %in% names(data)) {
      # Calcular media y mediana si estan activadas
      meanlabel <- if (s_mean) mean(data[[contvar]], na.rm = TRUE) else NULL
      medianlabel <- if (s_median) median(data[[contvar]], na.rm = TRUE) else NULL
      captionmean <- if (s_mean) "Linea roja: Media" else NULL
      captionmedian<- if (s_median) "Linea azul: Mediana" else NULL

      # Obtener la etiqueta de la variable o usar el nombre de la variable si no hay etiqueta
      lab_graf <- if (!is.null(label(data[[contvar]]))) label(data[[contvar]]) else contvar

      # Crear la grafica
      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[contvar]]))

      # Agregar densidad con argumentos personalizados
      p <- p + do.call(ggplot2::geom_density, densplot_args)

      # Agregar lineas para la media y mediana si estan activadas
      if (!is.null(meanlabel)) {
        p <- p + ggplot2::geom_vline(xintercept = meanlabel,
                                     color = c_mean,
                                     linetype=lt_mean,
                                     linewidth = lw_mean)
      }
      if (!is.null(medianlabel)) {
        p <- p + ggplot2::geom_vline(xintercept = medianlabel,
                                     color = c_median,
                                     linetype = lt_median,
                                     linewidth = lw_median)
      }

      # Etiquetas y personalizacion
      p <- p +
        ggplot2::labs(
          title = paste("Distribucion de", lab_graf),
          x = lab_graf,
          y = "Densidad",
          caption = paste(captionmean, "\n", captionmedian)
        ) +
        theme_func() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
        )

      # Almacenar la grafica en la lista
      grafdenscon[[contvar]] <- p
    } else {
      cat("\nLa variable", contvar, "no esta presente en la base de datos.\n")
    }
  }

  # Retornar la lista de graficas
  return(grafdenscon)
}




