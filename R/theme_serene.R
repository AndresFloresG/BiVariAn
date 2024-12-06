#' @import ggprism
#' @import ggplot2
#' @import dplyr
#'
#' @name theme_serene
#' @author Serene Chessur
#' @title Basic theme for Bivaran packages plots
#' @aliases theme_serene
#' @description
#' Basic theme for Bivaran packages plots
#'
#'
#' @export

theme_serene<- function(base_size = 14,
                      base_family = "sans",
                      base_fontface = "plain",
                      base_line_size = base_size / 14,
                      base_rect_size = base_size / 14,
                      axis_text_angle = 0,
                      border = F) {
  ggprism::theme_prism(palette = "black_and_white",
              base_size = base_size,
              base_family = base_family,
              base_fontface = base_fontface,
              base_line_size = base_line_size,
              base_rect_size = base_rect_size,
              axis_text_angle = axis_text_angle,
              border = border) %+replace%
    theme(panel.background = element_rect(fill = "white",
                                          colour = NA),
          plot.background = element_rect(fill = "white",
                                         colour = NA),
          axis.line = element_line(colour = "black", lineend = "butt"),
          axis.ticks = element_line(colour = "black"),
          panel.grid.major = element_line(colour="grey",
                                          linewidth=0.3,
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour = "grey",
                                          linewidth = 0.3,
                                          linetype = "dotted"),
          plot.caption = element_text(family="sans",
                                      face = "italic",
                                      size = 8,
                                      hjust = 1),
          plot.title = element_text(family="sans",
                                    face="plain",
                                    size = 12),
          legend.box.background = element_rect(),
          legend.box.margin = margin(6,6,6,6),
          legend.title = element_text(),
          panel.border = element_rect(linewidth = 1, fill=NA))
}
