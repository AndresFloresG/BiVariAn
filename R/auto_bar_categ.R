#' @import ggplot2
#' @import ggprism
#' @importFrom scales percent
#' @importFrom magrittr '%>%'
#' @name auto_bar_categ
#' @title Automatic generation of barplot with percentages
#' @param data Name of the dataframe
#' @param groupvar Name of the grouping variable. Grouping variable will be used in "fill" for aesthetics argument in the creation of each ggplot object. If not provided, the function take each variable as grouping and does not display the "fill" legend.
#' @param bar_args List of arguments to be passed to "geom_bar"
#' @param theme_func Theme of the generated plots
#' @param lang_labs Language of displayed labels. If null, default is spanish.

#' @export
#'
#'

auto_bar_categ <- function(data,
                           groupvar = NULL,
                           bar_args = list(),
                           theme_func = theme_serene,
                           lang_labs = c("EN", "SPA")) {
  # Verificar que el argumento de tema sea una funcion valida
  if (!is.function(theme_func)) {
    stop("El argumento 'theme_func' debe ser una funcion de tema valida.")
  }

  if (length(bar_args) == 0) bar_args=list(position = "dodge",
                                   colour = "black",
                                   linewidth = 0.9,
                                   alpha = 0.5) else bar_args

  if (any(is.null(lang_labs) | lang_labs == "SPA")){
    titlelab = "Distribuci\u00f3n de"
    ylabs= "Frecuencia"
  } else if(lang_labs == "EN"){
    titlelab = "Distribution of"
    ylabs= "Frequency"
  }

  if(!is.null(groupvar)){
  categ_var <- colnames(data)[sapply(data, function(x) is.factor(x) && !identical(x, data[[groupvar]]))]
  } else {
    categ_var <- colnames(data)[sapply(data, function(x) is.factor(x))]
  }

  # Seleccionar variables categoricas distintas de groupvar

  # Crear una lista para almacenar las graficas
  graficas <- list()

  # Bucle para generar y almacenar graficas
  for (varcat in categ_var) {
    if (varcat %in% names(data)) {  # Verifica si la variable esta en la base de datos
      # Obtener etiquetas o usar nombres de las variables
      lab_graf_cat <- if (!is.null(label(data[[varcat]]))) label(data[[varcat]]) else varcat

      if(!is.null(groupvar)){
        lab_graf_group <- if (!is.null(label(data[[groupvar]]))) label(data[[groupvar]]) else groupvar
      } else {
        lab_graf_group <- if (!is.null(label(data[[varcat]]))) label(data[[varcat]]) else varcat
      }

      if(is.null(groupvar)){
        group_fill = varcat
      } else group_fill = groupvar

      # Crear la grafica
      p <- ggplot(data, aes_string(x = varcat, fill = group_fill)) +
        do.call(geom_bar, bar_args) +
        geom_text(stat = "count",
                  aes(label=scales::percent(after_stat(count/sum(count)))),
                  position=position_dodge(width=0.9),
                  vjust=-.5)+
        labs(
          title = paste(titlelab, lab_graf_cat),
          x = lab_graf_cat,
          y = ylabs,
          fill = lab_graf_group
        ) +
        scale_y_continuous(((max(proportions(table(data[[varcat]], data[[groupvar]]))))*100)+5)+
        theme_func() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
        )

      if(is.null(groupvar)){
        p <- p + theme(legend.position = "none")
      }


      # Guardar la grafica en la lista
      graficas[[varcat]] <- p
    } else {
      if (any(is.null(lang_labs) | lang_labs == "SPA")){
        cat("\nLa variable", varcat, "no esta presente en la base de datos.\n")
      } else if (lang_labs == "EN"){
        cat("\nVariable", varcat, "is not in the provided dataframe \n")
      }
    }
  }

  # Retornar la lista de graficas
  return(graficas)
}
