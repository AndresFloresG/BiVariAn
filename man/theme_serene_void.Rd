% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_serene_void.R
\name{theme_serene_void}
\alias{theme_serene_void}
\title{Void theme for Bivaran packages plots}
\usage{
theme_serene_void(
  base_size = 11,
  base_family = "sans",
  base_fontface = "plain",
  base_line_size = base_size/22,
  base_rect_size = base_size/2,
  axis_text_angle = 0,
  border = FALSE
)
}
\arguments{
\item{base_size}{base font size, given in pts.}

\item{base_family}{base font family}

\item{base_fontface}{base font face}

\item{base_line_size}{base line size}

\item{base_rect_size}{base rect size}

\item{axis_text_angle}{Axis text angle}

\item{border}{Logical operator to indicate if the border should be printed}
}
\value{
Returns a list of classes "gg" and "theme"
}
\description{
Basic theme for Bivaran packages plots
}
\examples{
library(ggplot2)

data <- mtcars
p1 <- ggplot(data, aes(disp, hp))+
geom_point()+
geom_smooth()

p1 + theme_serene_void()


}
\author{
Jhoselin Marian Castro-Rodriguez
}
