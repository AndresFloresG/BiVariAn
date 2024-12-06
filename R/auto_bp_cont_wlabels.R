#' @import ggplot2
#' @import ggprism
#' @import table1
#' @name auto_bp_cont_wlabels
#' @author JMCR
#' @title auto_bp_cont_wlabels
#' @aliases auto_bp_cont_wlabels
#' @description
#' #' Automatically generates boxplot plots of continuous variables from a database and a grouping variable. The names of the variables are set to the names defined in the database. As a result, graphs generated with the default theme "theme_serene" will be obtained. In this function, the user must define each variable label with "label" function from "table1" package.
#' @param data Name of the dataframe
#' @param groupvar Name of the grouping variable
#'
#' @export

auto_bp_cont_wlabels<-function(data, groupvar){
  variables_continuas <- colnames(data %>% select_if(is.numeric))

  # Crear una lista para almacenar las graficas
  graficascont <- list()

  # Bucle para generar y almacenar graficas
  for (var2 in variables_continuas) {
    if (var2 %in% names(data)) {  # Verifica si la variable esta en la base de datos
      p <- ggplot2::ggplot(data, aes_string(x = data[[groupvar]], y=data[[var2]])) +
        ggplot2::geom_boxplot(colour = "black",
                     linewidth = 0.9,
                     alpha = 0.5) +
        ggplot2::labs(title = paste("Distribucion de", label(data[[var2]])),
             x = paste(label(data[[groupvar]])),
             y = var2) +
        theme_serene() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

      # Guardar la grafica en la lista
      graficascont[[var2]] <- p
    } else {
      cat("\nLa variable", var2, "no esta presente en la base de datos.\n")
    }
  }

  # Mostrar todas las graficas despues del bucle
  for (p in graficascont) {
    print(p)
  }
}
