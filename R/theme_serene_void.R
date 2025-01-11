#' @import ggprism
#' @import ggplot2
#' @import dplyr
#'
#' @name theme_serene_void
#' @author Jhoselin Marian Castro-Rodriguez
#' @title Void theme for Bivaran packages plots
#' @aliases theme_serene_void
#' @description
#' Basic theme for Bivaran packages plots
#' @param base_size base font size, given in pts.
#' @param base_family base font family
#' @param base_fontface base font face
#' @param base_line_size base line size
#' @param base_rect_size base rect size
#' @param axis_text_angle Axis text angle
#' @param border Logical operator to indicate if the border should be printed
#' @returns Returns a list of classes "gg" and "theme"
#'
#' @examples
#' library(ggplot2)
#'
#' data <- mtcars
#' p1 <- ggplot(data, aes(disp, hp))+
#' geom_point()+
#' geom_smooth()
#'
#' p1 + theme_serene_void()
#'
#'
#' @export

theme_serene_void<- function(base_size = 11,
                        base_family = "sans",
                        base_fontface = "plain",
                        base_line_size = base_size / 22,
                        base_rect_size = base_size / 2,
                        axis_text_angle = 0,
                        border = FALSE) {
  ggprism::theme_prism(palette = "black_and_white",
                       base_size = base_size,
                       base_family = base_family,
                       base_fontface = base_fontface,
                       base_line_size = base_line_size,
                       base_rect_size = base_rect_size,
                       axis_text_angle = axis_text_angle,
                       border = border) %+replace%
    theme(line = element_blank(),
          rect = element_blank(),
          text = element_text(family = base_family,
                              face = "plain",
                              colour = "black",
                              size = base_size,
                              lineheight = 0.9,
                              hjust = 0.5,
                              vjust = 0.5,
                              angle = 0),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks.length = unit(0, "pt"),
          axis.ticks.length.x = NULL,
          axis.ticks.length.x.top = NULL,
          axis.ticks.length.x.bottom = NULL,
          axis.ticks.length.y = NULL,
          axis.ticks.length.y.left = NULL,
          axis.ticks.length.y.right = NULL,
          axis.minor.ticks.length = unit(0,"pt"),
          legend.box = NULL,
          legend.key.size = unit(1.2,"lines"),
          legend.position = "right",
          legend.text = element_text(size = rel(0.8)),
          legend.title = element_text(hjust = 0),
          legend.key.spacing = unit(0,"pt"),
          legend.ticks.length = rel(0.2),
          strip.clip = "inherit",
          strip.text = element_text(size = rel(0.8)),
          panel.ontop = FALSE,
          panel.spacing = unit(0,"pt"),
          plot.margin = unit(c(0, 0, 0, 0), "lines"),
          plot.title = element_text(size = rel(1.2),
                                    hjust = 0,
                                    vjust = 1,
                                    margin = margin(t = 0)),
          plot.title.position = "panel",
          plot.subtitle = element_text(hjust = 0,
                                       vjust = 1,
                                       margin = margin(t = 0)),
          plot.caption = element_text(size = rel(0.8),
                                      hjust = 1,
                                      vjust = 1,
                                      margin = margin(t = 0)),
          plot.caption.position = "panel",
          plot.tag = element_text(size = rel(1.2),
                                  hjust = 0.5,
                                  vjust = 0.5),
          plot.tag.position = "topleft",
          complete = TRUE)
}
