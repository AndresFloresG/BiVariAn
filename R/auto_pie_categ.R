#' @import ggprism
#' @import ggplot2
#' @import dplyr
#'
#' @name auto_pie_categ
#' @aliases auto_pie_categ
#' @title Automatic generation of pieplots
#' @description
#' Generates pie plots based on categorical variables of a data frame.
#'
#' @param data Name of the dataframe
#' @param pie_bar_args List of arguments to be passed to "geom_bar"
#' @param theme_func Theme of the generated plots. Default is "theme_serene_void"
#' @param lang_labs Language of displayed labels. If null, default is spanish.
#' @param statistics Logical attribute to indicate if summary statistic parameters are shown.
#' @param stat_lab Statistics to be shown. Can choose if you want to show percentages or frequencies.
#' @param fill_grey Logical indicator to choose if the generated pie plots must be grey. Default is TRUE.
#'
#' @return Returns a list containing barplots as ggplot2 objects. Objects can be accessed via $ operator.
#'
#' @examples
#' data <- data.frame(categ = rep(c("Categ1", "Categ2"), 25),
#' var1 = rbinom(50, 2, prob = 0.3),
#' var2 = rbinom(50, 2, prob = 0.8),
#' var3 = rbinom(50, 2, prob = 0.7))
#' data$categ <- as.factor(data$categ)
#' data$var1 <- as.factor(data$var1)
#' data$var2 <- as.factor(data$var2)
#' data$var3 <- as.factor(data$var3)
#'
#' pieplot_list <- auto_pie_categ(data = data)
#'
#' # Call for all listed plots
#' pieplot_list
#'
#' # Call for one specific plot
#' pieplot_list$var1
#'
#' @export

auto_pie_categ <- function(data,
                           pie_bar_args = list(),
                           theme_func = theme_serene_void,
                           lang_labs = c("EN", "SPA"),
                           statistics = TRUE,
                           stat_lab = c("percent", "freq"),
                           fill_grey = TRUE) {

  if (!is.function(theme_func)) {
    stop("The argument 'theme_func' must be a valid theme function.")
  }

  if (any(is.null(stat_lab) | stat_lab == "percent")){
    stat_lab = "percent"
  } else stat_lab = stat_lab


  if (length(pie_bar_args) == 0) pie_bar_args <- list(stat = "identity",
                                                      colour = "black",
                                                      linewidth = 0.9,
                                                      alpha = 0.5)

  if (any(is.null(lang_labs) | lang_labs == "SPA")) {
    titlelab <- "Distribuci\u00f3n de"
    ylabs <- "Frecuencia"
  } else if (lang_labs == "EN") {
    titlelab <- "Distribution of"
    ylabs <- "Frequency"
  }

    categ_var <- colnames(data)[sapply(data, is.factor)]



  graphics <- list()

  for (varcat in categ_var) {
    if (varcat %in% names(data)) {

        freq <- table(data[[varcat]])
        prop <- freq / nrow(data)
        csum <- rev(cumsum(rev(freq)))
        pos <-  freq/2 + lead(csum, 1)
        pos <- if_else(is.na(pos), freq/2, pos)
        namescol <- NULL

        datastats <- data.frame(namescol = names(freq), freq = as.vector(freq), prop = as.vector(prop), csum = as.vector(csum), pos = as.vector(pos))


      lab_graf_cat <- if (!is.null(table1::label(data[[varcat]]))) table1::label(data[[varcat]]) else varcat

lab_graf_group <- if (!is.null(table1::label(data[[varcat]]))) table1::label(data[[varcat]]) else varcat


        p <- ggplot2::ggplot(datastats, ggplot2::aes(x = "", y = prop, fill = namescol))


        if (!length(pie_bar_args) == 0) {
        p <- p + do.call(geom_bar, pie_bar_args)
      } else p + do.call (pie_bar_args)

      if (statistics == TRUE && stat_lab == "percent"){
          p <- p + geom_text(aes(label = paste0(round(100 * prop, 2), "%")), position = position_stack(vjust = 0.5))
        } else if (statistics && stat_lab == "freq"){
          p <- p + geom_text(aes(label = paste0("N=",round(100 * freq, 2))), position = position_stack(vjust = 0.5))
        }


      p <- p + labs(
        title = paste(titlelab, lab_graf_cat),
        fill = lab_graf_group
      ) +
        theme_func() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
        )

      p <- p + coord_polar("y", start = 0)

      if(fill_grey){
        p <- p + scale_fill_grey()
      } else p


      graphics[[varcat]] <- p

    } else {
      if (any(is.null(lang_labs) | lang_labs == "SPA")) {
        cat("\nLa variable", varcat, "no est\u00e1 presente en la base de datos.\n")
      } else if (lang_labs == "EN") {
        cat("\nVariable", varcat, "is not in the provided dataframe.\n")
      }
    }
  }

  return(graphics) # Mover el return fuera del bucle
}

