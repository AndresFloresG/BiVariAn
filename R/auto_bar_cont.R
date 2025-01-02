#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom table1 label
#'
#' @name auto_bar_cont
#' @aliases auto_bar_cont
#' @title Automatic barplot of continous variables
#'
#' @description
#' Generates bar plots of contiuous variables based on numerical variables from a data frame. Internally, the function creates a tibble to summarize the data from each variable.
#'
#' @param data Name of the dataframe
#' @param groupvar Grouping variable
#' @param err_bar_show Logical indicator. Default TRUE show error bars in columns. Default is TRUE
#' @param err_bar Statistic to be shown as error bar. Can be "sd" for standard deviation or "se" for standard error. Defauult is "se".
#' @param lang_labs Language of the resulting plots. Can be "EN" for english or "SPA" for spanish. Default is "SPA"
#'
#' @return Returns a list containing barplots as ggplot2 objects. Objects can be accessed via $ operator.
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
#' # Create a list containing all the plots
#' barcontplots<-auto_bar_cont(data = data, groupvar = 'group', err_bar = "se", lang_labs = 'EN')
#'
#' # call to show all storaged plots
#' barcontplots
#'
#' # call to show one individual plots
#' barcontplots$var1
#'
#' @export

auto_bar_cont<-function(data,
                        groupvar,
                        err_bar_show = TRUE,
                        err_bar = c("sd", "se"),
                        lang_labs = c("EN", "SPA")) {
  if (!groupvar %in% names(data)) {
    stop("The grouping variable must be a column in the data frame.")
  }


  if (!is.factor(data[[groupvar]]) && !is.character(data[[groupvar]])) {
    stop("The grouping variable must be categorical.")
  }

  if(any(err_bar == "se" | is.null(err_bar))){
    err_bar = "se"
  }else err_bar


  if(any(is.null(lang_labs) | lang_labs == "SPA")){
      titlab1 = "Bar plot de"
      titlab2 = "por"
      caption = "Barras de error representan"
      captionerror = "error est\u00e1ndar"
      captionsdev = "desviaci\u00f3n est\u00e1ndar"
    } else if (lang_labs == "EN"){
      titlab1 = "Bar plot of"
      titlab2 = "by"
      caption = "Error bar represents"
      captionerror = "standard error"
      captionsdev = "standard deviation"
    }

  lab_groupvar <- if (!is.null(table1::label(data[[groupvar]]))){
    table1::label(data[[groupvar]])
  } else groupvar


  continuous_vars <- colnames(data %>% select(where(is.numeric)))


  plots <- list()

  for (var in continuous_vars) {

    lab_var <- if (!is.null(table1::label(data[[var]]))){
      table1::label(data[[var]])
    } else var


    # Resumir los datos
    SE_value <- mean_value <- sd_value <- NULL

    data_summary <- data %>%
      dplyr::group_by(.data[[groupvar]]) %>%
      dplyr::summarise(
        mean_value = mean(.data[[var]], na.rm = TRUE),
        sd_value = sd(.data[[var]], na.rm = TRUE),
        n_value = n(),
        SE_value = sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )

    p <- ggplot2::ggplot(data_summary, aes(x = .data[[groupvar]], y = mean_value)) +
      ggplot2::geom_col(fill="grey", color = "black", alpha = 0.8)+
      ggplot2::labs(
        y = paste(lab_var, "(Media)"),
        x = groupvar,
        title = paste(titlab1, lab_var, titlab2, lab_groupvar)
      ) +
      ggplot2::scale_y_continuous(limits = c(0, data_summary$mean_value + 5))+
      theme_serene()

    if (err_bar_show && err_bar == "se") {
      p <- p +
        ggplot2::geom_errorbar(data = data_summary, aes(
          ymin = mean_value - SE_value,
          ymax = mean_value + SE_value
        ), width = 0.2) +
        ggplot2::labs(caption = paste(caption, captionerror))
    } else if (err_bar_show && err_bar == "sd"){
      p <- p +
        ggplot2::geom_errorbar(data = data_summary, aes(
          ymin = mean_value - sd_value,
          ymax = mean_value + sd_value
        ), width = 0.2) +
        ggplot2::labs(caption = paste(caption, captionsdev))
    }

    plots[[var]] <- p
  }

  # Retornar la lista de gráficos
  return(plots)
}
