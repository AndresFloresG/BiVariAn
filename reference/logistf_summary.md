# Summary method for logistf with no printable output

Summary method for logistf models, currently this method is only used in
[step_bw_firth](https://andresfloresg.github.io/BiVariAn/reference/step_bw_firth.md)
function.

## Usage

``` r
logistf_summary(object, verbose = FALSE, ...)
```

## Arguments

- object:

  logistf class object

- verbose:

  logical. If TRUE, the output will be printed

- ...:

  Additional arguments

## Value

An object class 'data.frame' showing coefficients and p_values.

## References

Heinze G, Ploner M, Jiricka L, Steiner G. logistf: Firth’s Bias-Reduced
Logistic Regression. 2023. available on:
<https://CRAN.R-project.org/package=logistf>

## Examples

``` r
# Only use if you want a non-printable version of 'summary' for a logistfnp object.
if (requireNamespace("logistf")) {
  library(logistf)
  data <- mtcars
  data$am <- as.factor(data$am)

  regression_model <- logistf::logistf(am ~ mpg + cyl + disp, data = data)
  logistf_summary(regression_model)
}
```
