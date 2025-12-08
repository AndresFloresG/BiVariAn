# Bivariate analysis for correlation tests

Automatic correlation analyses for continuous variables with one
variable as reference. Variable names can be assigned using
[`table1::label()`](https://rdrr.io/pkg/table1/man/label.html) function.

## Usage

``` r
continuous_corr_test(
  data,
  referencevar,
  alternative = NULL,
  flextableformat = TRUE,
  corr_test = c("all", "pearson", "spearman", "kendall")
)
```

## Arguments

- data:

  Data frame from which variables will be extracted.

- referencevar:

  Reference variable. Must be a continuous variable.

- alternative:

  Alternative for cor.test. Must be either "two.sided", "geater" or
  "less"

- flextableformat:

  Logical operator to indicate the output desired. Default is TRUE. When
  FALSE, function will return a dataframe format. Because the function
  calculates different statistics for each correlation (specially in
  kendall correlation test), it may take some time to run. You can
  select individual variables using the pipe operator and the select
  function to run correlations only on the selected variables.

- corr_test:

  Correlation test to be performed

## Value

A dataframe or flextable containing pvalues for correlation tests along
with the normality and homocedasticity tests p values

## Examples

``` r
# example code

data <- data.frame(group = rep(letters[1:2], 15),
var1 = rnorm(30, mean = 15, sd = 5),
var2 = rnorm(30, mean = 20, sd = 2),
var3 = rnorm(30, mean = 10, sd = 1),
var4 = rnorm(30, mean = 5, sd =2))

data$group<-as.factor(data$group)

continuous_corr_test(data = data, referencevar = "var1", flextableformat = FALSE)
#>   Variable P_Shapiro_Resid P_Pearson P_Spearman P_Kendall   r_Pearson
#> 1     var2         0.73407   0.62153    0.58669   0.69717 -0.09392483
#> 2     var3         0.38925   0.19774    0.14690   0.10877  0.24192812
#> 3     var4         0.22638   0.68118    0.39157   0.47876  0.07821939
#>     r_P_CI_L  r_P_CI_H   t_Kendall    t_K_CI_L  t_K_CI_H rho_Spearman
#> 1 -0.4393280 0.2756727 -0.05287356 -0.29431440 0.1885673   -0.1030033
#> 2 -0.1296405 0.5539182  0.20919540 -0.06226355 0.4806544    0.2711902
#> 3 -0.2902285 0.4264707  0.09425287 -0.18977795 0.3782837    0.1617353
#>    rho_S_CI_L rho_S_CI_H
#> 1 -0.44669614  0.2671807
#> 2 -0.09872455  0.5752561
#> 3 -0.21081795  0.4932629

# Set names to variables
if(requireNamespace("table1")){
table1::label(data$var2) <- "Variable 2"
table1::label(data$var3) <- "Variable 3"
table1::label(data$var4) <- "Variable 4"

continuous_corr_test(data = data, referencevar = "var1", flextableformat = FALSE)
}
#>     Variable P_Shapiro_Resid P_Pearson P_Spearman P_Kendall   r_Pearson
#> 1 Variable 2         0.73407   0.62153    0.58669   0.69717 -0.09392483
#> 2 Variable 3         0.38925   0.19774    0.14690   0.10877  0.24192812
#> 3 Variable 4         0.22638   0.68118    0.39157   0.47876  0.07821939
#>     r_P_CI_L  r_P_CI_H   t_Kendall    t_K_CI_L  t_K_CI_H rho_Spearman
#> 1 -0.4393280 0.2756727 -0.05287356 -0.29431440 0.1885673   -0.1030033
#> 2 -0.1296405 0.5539182  0.20919540 -0.06226355 0.4806544    0.2711902
#> 3 -0.2902285 0.4264707  0.09425287 -0.18977795 0.3782837    0.1617353
#>    rho_S_CI_L rho_S_CI_H
#> 1 -0.44669614  0.2671807
#> 2 -0.09872455  0.5752561
#> 3 -0.21081795  0.4932629

# Example performing correlation test for only one variable
if(requireNamespace("dplyr")){
library(dplyr)
continuous_corr_test(data = data %>% select("var1","var2"),
 referencevar = "var1", flextableformat = FALSE, corr_test = "pearson")
}
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#>     Variable P_Shapiro_Resid P_Pearson   r_Pearson  r_P_CI_L  r_P_CI_H
#> 1 Variable 2         0.73407   0.62153 -0.09392483 -0.439328 0.2756727

# Example performing only pearson correlation test
continuous_corr_test(data = data, referencevar = "var1",
 flextableformat = FALSE, corr_test = "pearson")
#>     Variable P_Shapiro_Resid P_Pearson   r_Pearson   r_P_CI_L  r_P_CI_H
#> 1 Variable 2         0.73407   0.62153 -0.09392483 -0.4393280 0.2756727
#> 2 Variable 3         0.38925   0.19774  0.24192812 -0.1296405 0.5539182
#> 3 Variable 4         0.22638   0.68118  0.07821939 -0.2902285 0.4264707


```
