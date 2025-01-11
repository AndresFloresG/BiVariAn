#' @import ggplot2
#' @import ggprism
#' @importFrom table1 label
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
#' @param median_line_args Arguments to be passed to `geom_vline()` of plotted median line when `s_median = TRUE`. Default arguments are:
#' * color = "blue"
#' * linetype = "dotdash"
#' * linewidth = 1
#' @param mean_line_args Arguments to be passed to `geom_vline()` of plotted median line when `s_mean = TRUE`. Default arguments are:
#' * color = "red"
#' * linetype="solid"
#' * linewidth = 1
#' @param lang_labs Language of the resulting plots. Can be "EN" for english or "SPA" for spanish. Default is "SPA"
#' @returns Returns a list containing the generated density plots
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
#' densityplots <- auto_dens_cont(data = data)
#'
#' densityplots
#'
#' densityplots$var1
#'
#'
#' @export
#'

auto_dens_cont <- function(data,
                           s_mean = TRUE,
                           s_median = TRUE,
                           mean_line_args = list(),
                           median_line_args = list(),
                           densplot_args = list(),
                           theme_func = theme_serene,
                           lang_labs = c("EN", "SPA")) {
  # Verificar que el argumento de tema sea una funcion
  if (!is.function(theme_func)) {
    stop("El argumento 'theme_func' debe ser una funcion de tema valida.")
  }

  if(!is.data.frame(data) ){
    stop("data must be a data.frame object")
  }

  # Seleccionar variables continuas (numericas)
  contvariables <- colnames(data %>% select_if(is.numeric))

  if(length(contvariables) == 0){
    stop("data provided has no numerical variables")
  }

  if(any(is.null(lang_labs) | lang_labs == "SPA")){
    titlab1 = "Gr\u00e1fica de densidades de"
    captionmean_cap = "Linea roja: Media"
    captionmedian_cap = "Linea azul: Mediana"
    ylabs = "Densidad"
  } else if (lang_labs == "EN"){
    titlab1 = "Bar plot of"
    captionmean_cap = "Red line: Mean"
    captionmedian_cap = "Blue line: Median"
    ylabs = "Density"
  }


  default_mean_line_args <- list(
    color = "red",
    linetype = "solid",
    linewidth = 1
  )

  default_median_line_args <- list(
    color = "blue",
    linetype = "dotdash",
    linewidth = 1
  )

  if(length(mean_line_args) == 0 ){
    mean_line_args = default_mean_line_args
  } else {
    mean_line_args = modifyList(default_mean_line_args, mean_line_args)
  }

  if(length(median_line_args) == 0){
    median_line_args = default_median_line_args
  } else {
    median_line_args = modifyList(default_median_line_args, median_line_args)
  }

  grafdenscon <- list()

  for (contvar in contvariables) {
    if (contvar %in% names(data)) {
      # Calcular media y mediana si estan activadas
      meanlabel <- if (s_mean) mean(data[[contvar]], na.rm = TRUE) else NULL
      medianlabel <- if (s_median) median(data[[contvar]], na.rm = TRUE) else NULL
      captionmean <- if (s_mean) captionmean_cap else NULL
      captionmedian<- if (s_median) captionmedian_cap else NULL

      lab_graf <- if (!is.null(label(data[[contvar]]))) label(data[[contvar]]) else contvar

      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[contvar]]))

      p <- p + do.call(ggplot2::geom_density, densplot_args)


      if(s_mean) mean_line_args = modifyList(mean_line_args, list(xintercept = meanlabel))

      if(s_median) median_line_args = modifyList(median_line_args, list(xintercept = medianlabel))

      if (!is.null(meanlabel)) {
        p <- p + do.call(ggplot2::geom_vline, mean_line_args)
      }

      if (!is.null(medianlabel)) {
        p <- p + do.call(ggplot2::geom_vline, median_line_args)
      }

      # Etiquetas y personalizacion
      p <- p +
        ggplot2::labs(
          title = paste(titlab1, lab_graf),
          x = lab_graf,
          y = ylabs,
          caption = paste(captionmean, "\n", captionmedian)
        ) +
        theme_func() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
        )

      # Almacenar la grafica en la lista
      grafdenscon[[contvar]] <- p
    } else {
      warning("\nLa variable", contvar, "no esta presente en la base de datos.\n")
    }
  }

  # Retornar la lista de graficas
  return(grafdenscon)
}




