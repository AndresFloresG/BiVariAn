#' @import ggplot2
#' @import ggprism
#' @importFrom scales percent
#' @importFrom dplyr "%>%"
#' @name auto_bar_categ
#' @title auto_bar_categ
#'
#' @param data Name of the dataframe
#' @param groupvar Name of the grouping variable
#' @param bar_args List of arguments to be passed to "geom_bar"
#' @param theme_func Theme of the generated plots


#' @export
#'
#'

auto_bar_categ <- function(data,
                           groupvar,
                           bar_args = list(),
                           theme_func = theme_serene) {
  # Verificar que el argumento de tema sea una funcion valida
  if (!is.function(theme_func)) {
    stop("El argumento 'theme_func' debe ser una funcion de tema valida.")
  }

  if (length(bar_args) == 0) bar_args=list(position = "dodge",
                                   colour = "black",
                                   linewidth = 0.9,
                                   alpha = 0.5) else bar_args

  # Seleccionar variables categoricas distintas de groupvar
  categ_var <- colnames(data)[sapply(data, function(x) is.factor(x) && !identical(x, data[[groupvar]]))]

  # Crear una lista para almacenar las graficas
  graficas <- list()

  # Bucle para generar y almacenar graficas
  for (varcat in categ_var) {
    if (varcat %in% names(data)) {  # Verifica si la variable esta en la base de datos
      # Obtener etiquetas o usar nombres de las variables
      lab_graf_cat <- if (!is.null(label(data[[varcat]]))) label(data[[varcat]]) else varcat
      lab_graf_group <- if (!is.null(label(data[[groupvar]]))) label(data[[groupvar]]) else groupvar

      # Crear la grafica
      p <- ggplot(data, aes_string(x = varcat, fill = groupvar)) +
        do.call(geom_bar, bar_args) +
        geom_text(stat = "count",
                  aes(label=scales::percent(after_stat(count/sum(count)))),
                  position=position_dodge(width=0.9),
                  vjust=-.5)+
        labs(
          title = paste("Distribucion de", lab_graf_cat),
          x = lab_graf_cat,
          y = "Frecuencia",
          fill = lab_graf_group
        ) +
        theme_func() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
        )

      # Guardar la grafica en la lista
      graficas[[varcat]] <- p
    } else {
      cat("\nLa variable", varcat, "no esta presente en la base de datos.\n")
    }
  }

  # Retornar la lista de graficas
  return(graficas)
}
