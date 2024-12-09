
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BiVariAn

<!-- badges: start -->
<!-- badges: end -->

The purpose of BiVariAn is to facilitate bivariate analysis by
integrating functions that allow the results of such analyses to be
represented graphically and in text.

Currently the package can generate summary tables of analysis data for:
Analysis of continuous variables across two groups using Student’s
t-tests (OR Welch’s t-tests according to homogeneity of variances),
Mann-Whitney U; analysis of dichotomous variables using Chi-square and
Fisher’s exact test; analysis of normality of raw data using the
Shapiro-Wilk test.

It is also possible to generate graphs automatically with or without
labels on the variables with the possibility of choosing a theme from
the “ggplot2” package.

Currently under development is the option to generate a generic
interpretation of the statistical tests, temporarily available only in
Spanish, but there are plans to include more languages.

Note: The use of the package does not replace individual customized
analysis, but rather streamlines the process of producing the results.
We encourage users to perform statistical analyses based on the
recommendations for each type of study.

## Installation

You can install the development version of BiVariAn from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("AndresFloresG/BiVariAn")
```

## Example

Render an automatic Shapiro-Wilk’s table of a simple dataset

``` r
library(BiVariAn)
#> Warning: replacing previous import 'dplyr::lag' by 'stats::lag' when loading
#> 'BiVariAn'
#> Warning: replacing previous import 'dplyr::filter' by 'stats::filter' when
#> loading 'BiVariAn'

auto_shapiro_raw(cars)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

shapiro.test(cars$speed)
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  cars$speed
#> W = 0.97765, p-value = 0.4576
```

Render an automatic Shapiro-Wilk’s table of a more complex dataset

``` r
# Load riskCommunicator to access Framingham dataset
library(riskCommunicator)
#> Warning: package 'riskCommunicator' was built under R version 4.4.2
```

``` r
# Load dplyr to select specific columns
library(dplyr)
#> 
#> Adjuntando el paquete: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
data(cvdd)
```

``` r
# For shapiro.test, sample size must be between 3 and 5000
# Let's select only 300 observations (arbitrary)

set.seed(081224)
ex_sample<-slice_sample(cvdd, n=300)
```

``` r
# Now, let's select specific columns from the database
auto_shapiro_raw(ex_sample %>% select(TOTCHOL, SYSBP, DIABP, BMI, HEARTRTE))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r

# Common use of shapiro.test
shapiro.test(ex_sample$TOTCHOL)
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  ex_sample$TOTCHOL
#> W = 0.98654, p-value = 0.007891
```
