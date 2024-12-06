#' @import ggplot2
#' @import ggprism

auto_bp_cont<-function(data, groupvar){
  variables_continuas <- colnames(data %>% select_if(is.numeric))

  # Crear una lista para almacenar las gaficas
  graficascont <- list()

  # Bucle para generar y almacenar graficas
  for (var2 in variables_continuas) {
    if (var2 %in% names(data)) {  # Verifica si la variable esta en la base de datos
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = data[[groupvar]], y=data[[var2]])) +
        ggplot2::geom_boxplot(colour = "black",
                     linewidth = 0.9,
                     alpha = 0.5) +
        ggplot2::labs(title = paste("Distribución de", var2),
             x = groupvar,
             y = var2) +
        theme_serene() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

      # Guardar la grafica en la lista
      graficascont[[var2]] <- p
    } else {
      cat("\nLa variable", var2, "no está presente en la base de datos.\n")
    }
  }

  # Mostrar todas las graficas después del bucle
  for (p in graficascont) {
    print(p)
  }
}


