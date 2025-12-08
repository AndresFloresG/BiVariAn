# Stepwise backward for logistic Firth regression with automated dummy variables conversion (with forced terms)

Extension code to perform stepwise backward on a logistf model with
categorical variables. Automatically transforms predictors that are
factors into dummy variables.

## Usage

``` r
step_bw_firth(
  reg_model,
  s_lower = "~1",
  s_upper = "all",
  trace = TRUE,
  steps = NULL,
  p_threshold = 0.05,
  data = NULL,
  forced = NULL
)
```

## Arguments

- reg_model:

  A logistf model object.

- s_lower:

  Lower step. Not used; included for compatibility. Default = "~1".

- s_upper:

  Upper step. Not used; included for compatibility. Default = "all".

- trace:

  Logical. Print each step. Default TRUE.

- steps:

  Maximum number of elimination steps. If NULL, set to number of
  predictors.

- p_threshold:

  P-value threshold for elimination. Default = 0.05.

- data:

  Data frame. If NULL, extracted from reg_model.

- forced:

  Character vector of term names to always keep. Default NULL.

## Value

An object of class "step_bw" with components:

- final_model: the fitted logistf model

- steps: a data.frame of each elimination step

## References

Heinze G, Ploner M, Jiricka L, Steiner G. logistf: Firth’s Bias-Reduced
Logistic Regression. 2023.

Efroymson MA. Multiple regression analysis. In: Ralston A, Wilf HS,
editors. Mathematical methods for digital computers. New York: Wiley;
1960.

Ullmann T, Heinze G, Hafermann L, Schilhart-Wallisch C, Dunkler D, et
al. (2024) Evaluating variable selection methods for multivariable
regression models: A simulation study protocol. PLOS ONE 19(8): e0308543

## Examples

``` r
if (requireNamespace("logistf", quietly = TRUE)) {
  library(logistf)
  data <- mtcars
  data$am <- as.factor(data$am)
  regression_model <- logistf(am ~ mpg + cyl + disp, data = data)
  # Perform backward stepwise, forcing 'cyl' to remain
  stepwise <- step_bw_firth(
    regression_model,
    forced = c("cyl"),
    p_threshold = 0.05,
    trace = FALSE
  )
  final_model <- stepwise$final_model
  # Show steps and summary
  stepwise$steps
  summary(final_model)
}
#> logistf::logistf(formula = new_formula, data = dataprov)
#> 
#> Model fitted by Penalized ML
#> Coefficients:
#>                   coef  se(coef) lower 0.95    upper 0.95     Chisq          p
#> (Intercept)  0.8201147 1.8232397 -2.8921363  4.6412094266 0.1928272 0.66057392
#> cyl          0.5694572 0.6299805 -0.6794237  2.0101418441 0.7491163 0.38675617
#> disp        -0.0221055 0.0120472 -0.0526668 -0.0004207133 4.0277557 0.04475744
#>             method
#> (Intercept)      2
#> cyl              2
#> disp             2
#> 
#> Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
#> 
#> Likelihood ratio test=12.26211 on 2 df, p=0.002174283, n=32
#> Wald test = 7.246462 on 2 df, p = 0.02669628
```
