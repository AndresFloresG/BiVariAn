# auto_dens_cont

\#' Automatically generates density plots of continuous variables from a
database. The names of the variables are set to the names defined in the
database. As a result, graphs generated with the default theme
"theme_serene" will be obtained. In this function, the user must define
each variable label with "label" function from "table1" package.

## Usage

``` r
auto_dens_cont(
  data,
  s_mean = TRUE,
  s_median = TRUE,
  mean_line_args = list(),
  median_line_args = list(),
  densplot_args = list(),
  theme_func = theme_serene,
  lang_labs = c("EN", "SPA")
)
```

## Arguments

- data:

  Name of the dataframe

- s_mean:

  Show mean. Logical operator to indicate if the mean should be plotted.
  Default is TRUE

- s_median:

  Show median. Logical operator to indicate if the median should be
  plotted. Default is TRUE

- mean_line_args:

  Arguments to be passed to `geom_vline()` of plotted median line when
  `s_mean = TRUE`. Default arguments are:

  - color = "red"

  - linetype="solid"

  - linewidth = 1

- median_line_args:

  Arguments to be passed to `geom_vline()` of plotted median line when
  `s_median = TRUE`. Default arguments are:

  - color = "blue"

  - linetype = "dotdash"

  - linewidth = 1

- densplot_args:

  List of arguments to be passed to "geom_density"

- theme_func:

  Theme to display plots. Default is "theme_serene"

- lang_labs:

  Language of the resulting plots. Can be "EN" for english or "SPA" for
  spanish. Default is "SPA"

## Value

Returns a list containing the generated density plots

## Author

JMCR

## Examples

``` r
data <- data.frame(group = rep(letters[1:2], 30),
var1 = rnorm(30, mean = 15, sd = 5),
var2 = rnorm(30, mean = 20, sd = 2),
var3 = rnorm(30, mean = 10, sd = 1),
var4 = rnorm(30, mean = 5, sd =2))

data$group<-as.factor(data$group)

densityplots <- auto_dens_cont(data = data)

densityplots
#> $var1

#> 
#> $var2

#> 
#> $var3

#> 
#> $var4

#> 

densityplots$var1


```
