# Bivariate analysis for 2 groups

Automatic test for continuous variables for 2 groups. Variable names can
be assigned using
[`table1::label()`](https://rdrr.io/pkg/table1/man/label.html) function.

## Usage

``` r
continuous_2g(
  data,
  groupvar,
  ttest_args = list(),
  wilcox_args = list(),
  flextableformat = TRUE,
  caption = FALSE
)
```

## Arguments

- data:

  Data frame from which variables will be extracted.

- groupvar:

  Grouping variable as character. Must have exactly 2 levels.

- ttest_args:

  Arguments to be passed to
  [`t.test()`](https://rdrr.io/r/stats/t.test.html) function.

- wilcox_args:

  Arguments to be passed to
  [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) function.

- flextableformat:

  Logical operator to indicate the output desired. Default is TRUE. When
  FALSE, function will return a dataframe format.

- caption:

  TRUE/FALSE or character. If FALSE, no caption will be displayed. If
  TRUE, caption will be the same as groupvar or groupvar label. If
  character, character will be displayed as caption. For display
  options, flextableformat option must be TRUE.

## Value

Returns a dataframe or flextable of 2 groups 2 sided Mann Whitney's U or
T test, along with Shapiro-Wilk's p values and Levene's p value.

## See also

`vignette("continuous_2g", package="BiVariAn")`

## Examples

``` r
 df <- mtcars
 df$am <- as.factor(df$am)
 continuous_2g(data = df,
 groupvar = "am",
 flextableformat = FALSE)
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#>    Variable P_Shapiro_Resid P_Levene P_T_Test Var_Equal P_Mann_Whitney
#> 1       mpg         0.85734  0.04957  0.00137     FALSE        0.00187
#> 2       cyl         <0.001*  0.96551  0.00215      TRUE         0.0039
#> 3      disp         0.22478  0.17931  <0.001*      TRUE        <0.001*
#> 4        hp         0.01338  0.67085  0.17983      TRUE         0.0457
#> 5      drat         0.02818  0.71856  <0.001*      TRUE        <0.001*
#> 6        wt         0.01854  0.98911  <0.001*      TRUE        <0.001*
#> 7      qsec         0.28471  0.57490  0.20566      TRUE        0.26574
#> 8        vs         <0.001*  0.61223  0.35704      TRUE         0.3602
#> 9      gear         <0.001*  0.29718  <0.001*      TRUE        <0.001*
#> 10     carb         0.00185  0.22233  0.75445      TRUE        0.73502
#>    Diff_Means  CI_Lower  CI_Upper Significant_test
#> 1    -7.24494 -11.28019  -3.20968     Welch T test
#> 2     1.87045   0.73267   3.00822    Mann-W-U test
#> 3   146.84818  72.15611 221.54025   Student T test
#> 4    33.41700 -16.27768  83.11169    Mann-W-U test
#> 5    -0.76368  -1.04394  -0.48343    Mann-W-U test
#> 6     1.35789   0.83043   1.88536    Mann-W-U test
#> 7     0.82316  -0.47636   2.12268             None
#> 8    -0.17004  -0.54129   0.20121             None
#> 9    -1.17409  -1.50920  -0.83898    Mann-W-U test
#> 10   -0.18623  -1.39118   1.01871             None

# Set names to variables
if(requireNamespace("table1")){
table1::label(df$mpg) <- "Miles per gallon"
table1::label(df$cyl) <- "Number of cylinders"
table1::label(df$disp) <- "Displacement"
table1::label(df$hp) <- "Gross horsepower"
table1::label(df$drat) <- "Rear axle ratio"

continuous_2g(data = df, groupvar = "am", flextableformat = FALSE)
}
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#>               Variable P_Shapiro_Resid P_Levene P_T_Test Var_Equal
#> 1     Miles per gallon         0.85734  0.04957  0.00137     FALSE
#> 2  Number of cylinders         <0.001*  0.96551  0.00215      TRUE
#> 3         Displacement         0.22478  0.17931  <0.001*      TRUE
#> 4     Gross horsepower         0.01338  0.67085  0.17983      TRUE
#> 5      Rear axle ratio         0.02818  0.71856  <0.001*      TRUE
#> 6                   wt         0.01854  0.98911  <0.001*      TRUE
#> 7                 qsec         0.28471  0.57490  0.20566      TRUE
#> 8                   vs         <0.001*  0.61223  0.35704      TRUE
#> 9                 gear         <0.001*  0.29718  <0.001*      TRUE
#> 10                carb         0.00185  0.22233  0.75445      TRUE
#>    P_Mann_Whitney Diff_Means  CI_Lower  CI_Upper Significant_test
#> 1         0.00187   -7.24494 -11.28019  -3.20968     Welch T test
#> 2          0.0039    1.87045   0.73267   3.00822    Mann-W-U test
#> 3         <0.001*  146.84818  72.15611 221.54025   Student T test
#> 4          0.0457   33.41700 -16.27768  83.11169    Mann-W-U test
#> 5         <0.001*   -0.76368  -1.04394  -0.48343    Mann-W-U test
#> 6         <0.001*    1.35789   0.83043   1.88536    Mann-W-U test
#> 7         0.26574    0.82316  -0.47636   2.12268             None
#> 8          0.3602   -0.17004  -0.54129   0.20121             None
#> 9         <0.001*   -1.17409  -1.50920  -0.83898    Mann-W-U test
#> 10        0.73502   -0.18623  -1.39118   1.01871             None
```
