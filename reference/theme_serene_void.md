# Void theme for Bivaran packages plots

Basic theme for Bivaran packages plots

## Usage

``` r
theme_serene_void(
  base_size = 11,
  base_family = "sans",
  base_fontface = "plain",
  base_line_size = base_size/22,
  base_rect_size = base_size/2,
  axis_text_angle = 0,
  border = FALSE
)
```

## Arguments

- base_size:

  base font size, given in pts.

- base_family:

  base font family

- base_fontface:

  base font face

- base_line_size:

  base line size

- base_rect_size:

  base rect size

- axis_text_angle:

  Axis text angle

- border:

  Logical operator to indicate if the border should be printed

## Value

Returns a list of classes "gg" and "theme"

## Author

Jhoselin Marian Castro-Rodriguez

## Examples

``` r
library(ggplot2)

data <- mtcars
p1 <- ggplot(data, aes(disp, hp))+
geom_point()+
geom_smooth()

p1 + theme_serene_void()
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


```
