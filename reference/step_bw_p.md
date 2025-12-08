# Automatized stepwise backward for regression models (with forced terms)

Automatized stepwise backward for regression models (with forced terms)

## Usage

``` r
step_bw_p(
  reg_model,
  s_lower = "~1",
  s_upper = "all",
  trace = TRUE,
  steps = NULL,
  p_threshold = 0.05,
  data = NULL,
  forced = NULL,
  ...
)
```

## Arguments

- reg_model:

  Regression model. Must be a glm or lm object

- s_lower:

  Lower step. Names of the variables to be included at the lower step.
  Default is "~1" (intercept only)

- s_upper:

  Upper step. Names of the variables to be included at the upper step.
  Default is "all" (all predictors)

- trace:

  Logical. Whether to print each step to the console. Default is TRUE.

- steps:

  Maximum number of elimination steps. If NULL, set to number of
  predictors.

- p_threshold:

  P-value threshold for elimination. Default is 0.05.

- data:

  Data frame for stepwise. If NULL, extracted from reg_model.

- forced:

  Character vector of predictor names to always keep. Default NULL.

- ...:

  Additional arguments passed to car::Anova().

## Value

An object of class "step_bw" containing the final model and a data.frame
of steps.

## References

Efroymson MA. Multiple regression analysis. In: Ralston A, Wilf HS,
editors. Mathematical methods for digital computers. New York: Wiley;
1960.

## Examples

``` r
data(mtcars)
# Fit a linear model
regression_model <- lm(cyl ~ ., data = mtcars)
# Perform backward stepwise, forcing "wt" and "hp" to remain
stepwise <- step_bw_p(
  regression_model,
  forced = c("wt", "hp"),
  trace = FALSE
)
# Extract final model and view summary
final_model <- stepwise$final_model
summary(final_model)
#> 
#> Call:
#> lm(formula = cyl ~ hp + wt + vs + gear, data = mtcars)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.24880 -0.44498  0.00955  0.31970  0.99432 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  7.05607    1.10180   6.404 7.35e-07 ***
#> hp           0.01248    0.00271   4.606 8.78e-05 ***
#> wt           0.17468    0.19275   0.906 0.372829    
#> vs          -1.23469    0.30673  -4.025 0.000414 ***
#> gear        -0.73791    0.19439  -3.796 0.000757 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.5853 on 27 degrees of freedom
#> Multiple R-squared:  0.9065, Adjusted R-squared:  0.8926 
#> F-statistic:  65.4 on 4 and 27 DF,  p-value: 1.702e-13
#> 
```
