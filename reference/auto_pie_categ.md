# Automatic generation of pieplots

Generates pie plots based on categorical variables of a data frame.

## Usage

``` r
auto_pie_categ(
  data,
  pie_bar_args = list(),
  theme_func = theme_serene_void,
  lang_labs = c("EN", "SPA"),
  statistics = TRUE,
  stat_lab = c("percent", "freq"),
  fill_grey = TRUE
)
```

## Arguments

- data:

  Name of the dataframe

- pie_bar_args:

  List of arguments to be passed to "geom_bar"

- theme_func:

  Theme of the generated plots. Default is "theme_serene_void"

- lang_labs:

  Language of displayed labels. If null, default is spanish.

- statistics:

  Logical attribute to indicate if summary statistic parameters are
  shown.

- stat_lab:

  Statistics to be shown. Can choose if you want to show percentages or
  frequencies.

- fill_grey:

  Logical indicator to choose if the generated pie plots must be grey.
  Default is TRUE.

## Value

Returns a list containing barplots as ggplot2 objects. Objects can be
accessed via \$ operator.

## Examples

``` r
data <- data.frame(categ = rep(c("Categ1", "Categ2"), 25),
var1 = rbinom(50, 2, prob = 0.3),
var2 = rbinom(50, 2, prob = 0.8),
var3 = rbinom(50, 2, prob = 0.7))
data$categ <- as.factor(data$categ)
data$var1 <- as.factor(data$var1)
data$var2 <- as.factor(data$var2)
data$var3 <- as.factor(data$var3)

pieplot_list <- auto_pie_categ(data = data)

# Call for all listed plots
pieplot_list
#> $categ

#> 
#> $var1

#> 
#> $var2

#> 
#> $var3

#> 

# Call for one specific plot
pieplot_list$var1

```
