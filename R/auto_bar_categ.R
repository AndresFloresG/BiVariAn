#' @import ggplot2
#' @import ggprism
#' @importFrom table1 label
#' @importFrom scales percent
#' @importFrom magrittr '%>%'
#' @name auto_bar_categ
#' @title Automatic generation of barplot with percentages
#' @description
#' Automatically generates barplot stratified by group variables with or without percentages.
#'
#' @param data Name of the dataframe
#' @param groupvar Name of the grouping variable. Grouping variable will be used in "fill" for aesthetics argument in the creation of each ggplot object. If not provided, the function take each variable as grouping and does not display the "fill" legend.
#' @param bar_args List of arguments to be passed to "geom_bar". If `NULL`, the function uses default arguments such as:
#' * position = "dodge"
#' * colour = "black"
#' * linewidth = 0.9
#' * alpha = 0.5
#' @param theme_func Theme of the generated plots. Must be the name of the function without parenthesis. Use for example: `theme_minimal` instead of `theme_minimal()`
#' @param lang_labs Language of displayed labels. If null, default is spanish.
#' @param showpercent Logical atribute to indicate if the graph should include percentages
#' @param drop_na Remove NAs from provided dataframe
#'
#' @return Returns a list containing all barplots as ggplot object. Can be accessed via $ operator
#'
#' @examples
#' data<-data.frame(categ = rep(letters[1:2], 10),
#' var1 = rep(LETTERS[4:5], 10),
#' var2 = rep(LETTERS[6:7], 10),
#' var3 = rep(LETTERS[8:9], 10),
#' var4 = rep(LETTERS[10:11], 10))
#'
#' data$categ <- as.factor(data$categ)
#' data$var1 <- as.factor(data$var1)
#' data$var2 <- as.factor(data$var2)
#' data$var3 <- as.factor(data$var3)
#' data$var4 <- as.factor(data$var4)
#'
#' barplot_list<-auto_bar_categ(data = data, groupvar = "categ", lang_labs = "EN")
#'
#' barplot_list$var1
#'
#'
#' # Example using `groupvar` argument as `NULL`
#' auto_bar_categ(data = data)$var2
#'
#'
#' @export
auto_bar_categ <- function (data,
                            groupvar = NULL,
                            bar_args = NULL,
                            theme_func = theme_serene,
                            lang_labs = NULL,
                            showpercent = TRUE,
                            drop_na = TRUE) {
  if (!is.function(theme_func)) {
    stop("Argument 'theme_func' must be a valid theme function.")
  }
  if (!is.data.frame(data)) {
    stop("data must be a data.frame object")
  }
  if (!is.logical(showpercent) || length(showpercent) != 1L || is.na(showpercent)) {
    stop("showpercent must be a logical argument")
  }
  if (!is.logical(drop_na) || length(drop_na) != 1L || is.na(drop_na)) {
    stop("drop_na must be TRUE/FALSE")
  }

  if (is.null(lang_labs)) {
    lang_labs <- "SPA"
  } else {
    lang_labs <- match.arg(lang_labs, c("SPA", "EN"))
  }

  if (lang_labs == "SPA") {
    titlelab <- "Distribuci\u00f3n de"
    ylabs    <- "Frecuencia"
  } else {
    titlelab <- "Distribution of"
    ylabs    <- "Frequency"
  }

  if (!is.null(groupvar)) {
    if (!groupvar %in% names(data)) {
      stop("The grouping variable must be a column in the data frame.")
    }
    if (!is.factor(data[[groupvar]]) && !is.character(data[[groupvar]])) {
      stop("The grouping variable must be categorical.")
    }
  }

  bar_args_default <- list(position = "dodge", colour = "black",
                           linewidth = 0.9, alpha = 0.5)
  if (is.null(bar_args) || length(bar_args) == 0L) {
    bar_args <- bar_args_default
  } else {
    bar_args <- utils::modifyList(bar_args_default, bar_args)
  }
  if (isTRUE(drop_na) && is.null(bar_args$na.rm)) bar_args$na.rm <- TRUE

  if (!is.null(groupvar)) {
    categ_var <- colnames(data)[sapply(data, function(x) is.factor(x) && !identical(x, data[[groupvar]]))]
  } else {
    categ_var <- colnames(data)[sapply(data, is.factor)]
  }

  graficas <- list()

  for (varcat in categ_var) {
    if (!varcat %in% names(data)) {
      if (lang_labs == "SPA") {
        warning("\nLa variable ", varcat, " no est\u00e1 presente en la base de datos.\n")
      } else {
        warning("\nVariable ", varcat, " is not in the provided dataframe.\n")
      }
      next
    }

    df <- data
    if (isTRUE(drop_na)) {
      if (!is.null(groupvar)) {
        df <- df[!is.na(df[[varcat]]) & !is.na(df[[groupvar]]), , drop = FALSE]
      } else {
        df <- df[!is.na(df[[varcat]]), , drop = FALSE]
      }
      if (is.factor(df[[varcat]]))  df[[varcat]]  <- droplevels(df[[varcat]])
      if (!is.null(groupvar) && is.factor(df[[groupvar]])) df[[groupvar]] <- droplevels(df[[groupvar]])
    }

    if (nrow(df) == 0L) {
      if (lang_labs == "SPA") {
        warning("La variable ", varcat, " contiene solo NA; se omite.")
      } else {
        warning("Variable ", varcat, " has only NA; skipping.")
      }
      next
    }

    lab_graf_cat <- if (!is.null(table1::label(df[[varcat]]))) table1::label(df[[varcat]]) else varcat

    if (!is.null(groupvar)) {
      lab_graf_group <- if (!is.null(table1::label(df[[groupvar]]))) table1::label(df[[groupvar]]) else groupvar
      group_fill <- groupvar
    } else {
      lab_graf_group <- lab_graf_cat
      group_fill <- varcat
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[varcat]], fill = .data[[group_fill]])) +
      do.call(ggplot2::geom_bar, bar_args)

    if (isTRUE(showpercent)) {
      p <- p + ggplot2::geom_text(
        stat = "count",
        ggplot2::aes(label = scales::percent(after_stat(count/sum(count)))),
        position = ggplot2::position_dodge(width = 0.9),
        vjust = -0.5
      )
    }

    p <- p + ggplot2::labs(
      title = paste(titlelab, lab_graf_cat),
      x = lab_graf_cat, y = ylabs, fill = lab_graf_group
    )

    if (!is.null(groupvar)) {
      tab <- table(df[[varcat]], df[[groupvar]])
    } else {
      tab <- table(df[[varcat]])
    }
    ymax <- if (length(tab)) max(tab, na.rm = TRUE) + 10 else 10

    p <- p +
      ggplot2::scale_y_continuous(limits = c(0, ymax)) +
      ggplot2::scale_x_discrete(drop = TRUE, na.translate = FALSE) +
      ggplot2::scale_fill_discrete(na.translate = FALSE) +
      theme_func() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"))

    if (is.null(groupvar)) {
      p <- p + ggplot2::theme(legend.position = "none")
    }

    graficas[[varcat]] <- p
  }

  graficas
}
