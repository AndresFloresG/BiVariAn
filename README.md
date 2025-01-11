
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BiVariAn <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/AndresFloresG/BiVariAn/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AndresFloresG/BiVariAn/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/AndresFloresG/BiVariAn/graph/badge.svg)](https://app.codecov.io/gh/AndresFloresG/BiVariAn)
<!-- badges: end -->

<br/>

<br/>

# Overview

The purpose of ‘BiVariAn’ is to facilitate bivariate analysis by
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
the ‘ggplot2’ package.

Currently under development is the option to generate a generic
interpretation of the statistical tests, temporarily available only in
Spanish, but there are plans to include more languages.

In addition to bivariate analyses, regression analyses with predictor
selection using the stepwise backward method for linear, logistic and
Firth penalized logistic regression models can be performed in an
automated manner.

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

Or you can install the official CRAN version with:

``` r
install.packages("BiVariAn")
```

## Example

Loading the package

``` r
library(BiVariAn)
```

Render an automatic Shapiro-Wilk’s table of a simple dataset

``` r
auto_shapiro_raw(cars)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r

shapiro.test(cars$speed)
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  cars$speed
#> W = 0.97765, p-value = 0.4576
```

Return Shapiro-Wilk’s results as a dataframe

``` r
auto_shapiro_raw(cars, flextableformat = FALSE)
#>       Variable p_shapiro  Normality
#> speed    speed   0.45763     Normal
#> dist      dist    0.0391 Non-normal
```

Render an automatic Shapiro-Wilk’s table of a more complex dataset

``` r
# Load riskCommunicator to access Framingham dataset
library(riskCommunicator)
```

``` r
# Load dplyr to select specific columns
library(dplyr)
data(cvdd)
```

For shapiro.test, sample size must be between 3 and 5000

Let’s select only 300 observations (arbitrary)

``` r
set.seed(081224)
ex_sample<-slice_sample(cvdd, n=300)
```

Now, let’s select specific columns from the database

``` r

auto_shapiro_raw(ex_sample %>% select(TOTCHOL, SYSBP, DIABP, BMI, HEARTRTE))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

Common use of shapiro.test

``` r
shapiro.test(ex_sample$TOTCHOL)
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  ex_sample$TOTCHOL
#> W = 0.98654, p-value = 0.007891
```

Return the same Shapiro-Wilk’s results as a dataframe

``` r
auto_shapiro_raw(ex_sample %>% select(TOTCHOL, SYSBP, DIABP, BMI, HEARTRTE), flextableformat = FALSE)
#>          Variable p_shapiro  Normality
#> TOTCHOL   TOTCHOL   0.00789 Non-normal
#> SYSBP       SYSBP   <0.001* Non-normal
#> DIABP       DIABP   <0.001* Non-normal
#> BMI           BMI   <0.001* Non-normal
#> HEARTRTE HEARTRTE   <0.001* Non-normal
```
